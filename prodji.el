;;; prodji.el --- Helpers for managing and working on Django projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Joshua Munn

;; Author: Joshua Munn <public@elysee-munn.family>
;; URL: https://github.com/jams2/prodji/
;; Version: 0.1.0
;; Package-Requires: (virtualenvwrapper cl-lib)
;; Keywords: tools, processes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is used for managing work between multiple Django
;; projects. It assumes the use of docker-compose (with a
;; docker-compose.yml file in the project root), or the Django
;; development server (with a .env file in the project root,
;; containing a DJANGO_SETTINGS_MODULE entry). The project root must
;; be defined in $VIRTUAL_ENV_DIR/.project. The typical way to create
;; a compatible virtual environment is with the Python
;; virtualenvwrapper package, for example:
;;
;;	$ mkvirtualenv -a /path/to/project/root my-virtual-env
;;
;; When a project is "active", it will have one running server
;; process, one shell process, and the virtual environment will have
;; been activated using virtualenvwrapper's `venv-workon'.
;;
;; To activate a project, call `prodji'. You will be prompted for the
;; virtual environment name. Calling `prodji' with an environment
;; active will tear down that projects processes before activating the
;; new project. Some other functions are provided:
;;
;; - `prodji-restart-server' (restart the running server process)
;;
;; - `prodji-killall' (kill running processes, kill buffers with files
;; 	under project root)
;;
;; - `prodji-run-django-command' (run a Django management command in
;;	the shell process)
;;
;; - `prodji-goto-server' (jump to the server buffer)
;;
;; - `prodji-goto-shell' (jump to the shell buffer)
;;

;;; Code:

(require 'cl-lib)
(require 'virtualenvwrapper)

(defvar prodji-docker-buffer nil)
(defvar prodji-shell-buffer nil)
(defvar prodji-project-root nil)
(defvar prodji-django-server-buffer nil)

(defun concat-path (&rest parts)
  "Concatenate a file path, using expand-file-name to join PARTS intelligently.

e.g. (concat-path \"/etc\" \"nginx.conf.d\") -> \"/etc/nginx.conf.d\""
  (reduce (lambda (a b) (expand-file-name b a)) parts))

(defun prodji ()
  "Start work on a Django project.

Requires a Python virtual environment created with
virtualenvwrapper, with the project root set (this is stored in
the .project file in the virtual environment directory, when the
virtual environment is created with a -a flag). If a project is
already active, stop its processes and kill their buffers."
  (interactive)
  (let ((venv-name (venv-read-name
		    (if venv-current-name
			(format "Project venv (currently %s): " venv-current-name)
		      "Project venv: ")))
	(kill-buffer-query-functions nil))
    (when prodji-docker-buffer
      (prodji-teardown-docker nil))
    (when prodji-shell-buffer
      (prodji-teardown-shell nil))
    (when prodji-django-server-buffer
      (prodji-teardown-django-server nil))
    (setq prodji-project-root nil)
    (prodji-activate-venv venv-name))
  (let* ((shell-buffer (prodji-start-shell-process))
	 (project-file (expand-file-name ".project" venv-current-dir))
	 (project-root (with-temp-buffer
			 (insert-file-contents project-file)
			 (string-trim (buffer-string))))
	 (server-buffer (prodji-start-docker-or-django project-root)))
    (setq prodji-project-root project-root)
    (if server-buffer (switch-to-buffer server-buffer)
      (switch-to-buffer shell-buffer))))

(defun prodji-start-docker-or-django (project-root)
  (cond ((file-exists-p (concat-path project-root "docker-compose.yml"))
	 (prodji-start-docker-process project-root))
	((file-exists-p (concat-path project-root ".env"))
	 (prodji-start-django-server project-root))
	(t (user-error "Need docker-compose.yml or .env to run server process"))))

(defun prodji-activate-venv (venv-name)
  (venv-workon venv-name)
  (setq flycheck-python-pylint-executable
	(concat-path venv-current-dir venv-executables-dir "python"))
  (let ((settings (prodji-dot-env-get "DJANGO_SETTINGS_MODULE")))
    (when settings
      (setenv "DJANGO_SETTINGS_MODULE" settings))))

(defun prodji-killall ()
  (interactive)
  (when prodji-project-root
    (let ((kill-buffer-query-functions nil))
      (prodji-teardown-server)
      (prodji-teardown-shell)
      (call-interactively 'prodji-kill-project-buffers)
      (setq prodji-project-root nil)
      (venv-deactivate)
      (setq flycheck-python-pylint-executable "python"))))

(defun prodji-kill-project-buffers ()
  (interactive)
  (cl-loop
   for buf in (buffer-list)
   for file-name = (buffer-file-name buf)
   do (when (and
	     file-name
	     (string-match-p prodji-project-root
			     (buffer-file-name buf)))
	(kill-buffer buf))))

(defun prodji-teardown-server ()
  (cond (prodji-docker-buffer (prodji-teardown-docker nil))
	(prodji-django-server-buffer (prodji-teardown-django-server nil))
	(t (user-error "No active prodji server process"))))

(cl-defmacro prodji--teardown-process
    ((buffer &key show-progress sentinel) &rest how)
  (declare (indent defun))
  (let ((sentinel-not-nil (fboundp sentinel)))
    `(when ,buffer
       (let ,(append `((process (get-buffer-process ,buffer)))
		     (and show-progress
			  `((reporter
			     (make-progress-reporter
			      (format "Stopping %s..." ,(symbol-name buffer)))))))
	 (when ,sentinel-not-nil
	   (set-process-sentinel process
				 (lambda (proc output)
				   (,sentinel proc output)
				   (when ,show-progress
				     (progress-reporter-done reporter)))))
	 (with-current-buffer ,buffer ,@how)
	 (setq ,buffer nil)))))

(defun prodji--kill-buffer-when-finished (proc output)
  (when (string= output "finished\n")
    (kill-buffer (process-buffer proc))))

(defun prodji--noop-sentinel (proc output)
  "Prevent output from being inserted into proc buffer."
  nil)

(defun prodji-teardown-shell (&optional preserve-buffer)
  (prodji--teardown-process
    (prodji-shell-buffer :sentinel prodji--kill-buffer-when-finished)
    (comint-send-eof)))

(defun prodji-teardown-docker (&optional preserve-buffer)
  (prodji--teardown-process
    (prodji-docker-buffer :show-progress t
			  :sentinel prodji--kill-buffer-when-finished)
    (call-process-shell-command "docker-compose stop" nil nil nil)))

(defun prodji-teardown-django-server (&optional preserve-buffer)
  (prodji--teardown-process
    (prodji-django-server-buffer :sentinel prodji--kill-buffer-when-finished)
    (interrupt-process (get-buffer-process prodji-django-server-buffer))))

(defun prodji-start-shell-process ()
  (let ((shell-buffer (shell (format "*shell-%s*" venv-current-name))))
    (setq prodji-shell-buffer shell-buffer)))

(defun prodji-start-docker-process (working-directory)
  (let ((docker-buffer (get-buffer-create (format "*docker-%s*" venv-current-name))))
    (with-current-buffer docker-buffer
      (cd working-directory)
      (start-file-process
       (format "docker-compose:%s" venv-current-name)
       docker-buffer
       "docker-compose"
       "up"
       "--no-color")
      (prodji-docker-mode))
    (setq prodji-docker-buffer docker-buffer)))

(defun prodji-get-python-executable ()
  (concat-path
   venv-current-dir
   venv-executables-dir
   "python"))

(defun prodji-dot-env-get (var-name)
  "Get the value of VAR-NAME from project-root/.env or nil."
  (let ((dot-env-file (concat-path prodji-project-root ".env")))
    (when (not (file-readable-p dot-env-file))
      (error "File not readable: %s" (concat-path prodji-project-root ".env")))
    (with-temp-buffer
      (insert-file-contents dot-env-file)
      (goto-char (point-min))
      (if (search-forward-regexp (concat "^" var-name "="))
	  (buffer-substring-no-properties (point) (line-end-position))
	nil))))

(defun prodji-start-django-server (working-directory)
  "Start a django dev server process in WORKING-DIRECTORY.

Attempt to read a DJANGO_SETTINGS_MODULE value from project-root/.env"
  (let ((server-buffer (get-buffer-create
			(format "*django-%s*" venv-current-name))))
    (with-current-buffer server-buffer
      (cd working-directory)
      (start-file-process
       (format "django-server:%s" venv-current-name)
       server-buffer
       (prodji-get-python-executable)
       "manage.py"
       "runserver"
       "--no-color"
       "--settings"
       (prodji-dot-env-get "DJANGO_SETTINGS_MODULE"))
      (special-mode))
    (setq prodji-django-server-buffer server-buffer)))

(defun prodji-restart-server ()
  (interactive)
  (cond (prodji-docker-buffer (prodji-restart-docker))
	(prodji-django-server-buffer (prodji-restart-django-server))
	(t (user-error "No active prodji server process"))))

(defun prodji-restart-django-server ()
  (when prodji-django-server-buffer
    (prodji-teardown-django-server t))
  (pop-to-buffer-same-window
   (prodji-start-django-server prodji-project-root)))

(defun prodji-restart-docker ()
  (when prodji-docker-buffer
    (prodji-teardown-docker t))
  (pop-to-buffer-same-window
   (prodji-start-docker-process prodji-project-root)))

(defun prodji--get-management-command-prefix ()
  (or (and prodji-docker-buffer '("docker-compose" "run" "web"))
      '()))

(defun prodji-run-django-command ()
  (interactive)
  (when (not prodji-shell-buffer)
    (user-error "project not activated"))
  (let* ((command (completing-read
		   "Command: "
		   '("makemigrations"
		     "collectstatic"
		     "migrate"
		     "showmigrations"
		     "shell"
		     "test"
		     "createsuperuser")))
	 (buf (get-buffer-create "*prodji-management-command*"))
	 (program (append (prodji--get-management-command-prefix)
			  '("python" "manage.py")
			  `(,command))))
    (with-current-buffer buf
      (cd prodji-project-root)
      (delete-region (point-min) (point-max))
      (pop-to-buffer buf)
      (apply 'make-comint-in-buffer
	     (format "<prodji:%s>" command)
	     buf
	     (first program)
	     nil
	     (rest program)))))

(defun prodji-goto-server (prefix)
  (interactive "P")
  (cond (prodji-docker-buffer
	 (prodji--goto-buffer prodji-docker-buffer prefix))
	(prodji-django-server-buffer
	 (prodji--goto-buffer prodji-django-server-buffer prefix))
	(t (user-error "No active prodji server buffer"))))

(defun prodji-goto-shell (other-window)
  (interactive "P")
  (prodji--goto-buffer prodji-shell-buffer other-window))

(defun prodji--goto-buffer (buffer in-other-window)
  (cond ((not buffer) (user-error "no matching buffer"))
	(in-other-window (switch-to-buffer-other-window buffer))
	(t (switch-to-buffer buffer))))

(setq prodji-docker-mode-highlights
      '(("^[a-z]+_[0-9]+\s+|" . font-lock-type-face)
	("\\[Note\\]" . font-lock-variable-name-face)
	("\\[Warning\\]" . font-lock-warning-face)
	("\\([0-9]\\{2\\}\\(:\\|\\b\\)\\)\\{3\\}" . font-lock-function-name-face)))

(define-derived-mode prodji-docker-mode special-mode "django-docker"
  (setq font-lock-defaults '(prodji-docker-mode-highlights)))

(provide 'prodji)
;;; prodji.el ends here
