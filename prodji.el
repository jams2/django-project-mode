;;; prodji.el --- Helpers for managing and working on Django projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joshua Munn

;; Author: Joshua Munn <public@elysee-munn.family>
;; URL: https://github.com/jams2/prodji/
;; Version: 0.1.0
;; Package-Requires: (virtualenvwrapper cl-lib vterm shell subr-x comint)
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
;; projects. It assumes the use of docker compose (with a
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
;; 	under project root, deactivate virtualenv)
;;
;; - `prodji-run-django-command' (run a Django management command in
;;	the shell process)
;;
;; - `prodji-goto-server' (jump to the server buffer)
;;
;; - `prodji-goto-shell' (jump to the shell buffer)
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'virtualenvwrapper)
  (require 'vterm)
  (require 'shell)
  (require 'subr-x)
  (require 'comint))

(defvar prodji-shell-buffer nil)

(defvar prodji-project-root nil)

(defvar prodji-server-process-buffer nil)

(defvar prodji-venv-name nil)

(defvar prodji-venv-directory nil)

(defvar prodji-virtual-env-manager 'poetry
  "Method used for management of virtual environments.")

(defcustom prodji-run-server nil
  "Whether or not prodji should run the project's development server."
  :type '(boolean))

(defcustom prodji-project-directory (expand-file-name "~/projects")
  "Directory containing the user's projects"
  :type '(string))

(defcustom prodji-poetry-virtual-env-dir (expand-file-name "~/.cache/pypoetry/virtualenvs")
  "Directory that contains python virtual environment directories."
  :type '(string))

(defcustom prodji-create-pyright-config nil
  "Whether a pyrightconfig.json file should be created.

If `t', a pyrightconfig.json file will be created in the project
root, if it does not exist."
  :type '(boolean))

(defcustom prodji-docker-compose-executable "docker compose"
  "Docker compose executable.

Older versions use a `docker-compose' executable."
  :type '(string))

(cl-defstruct prodji-server type buffer)

(defun split-string-shell-command (string)
  "Split STRING (a shell command) into a list of strings.
General shell syntax, like single and double quoting, as well as
backslash quoting, is respected.

Borrowed from emacs git as not available in 27."
  (with-temp-buffer
    (insert string)
    (let ((comint-file-name-quote-list shell-file-name-quote-list))
      (car (shell--parse-pcomplete-arguments)))))

(defun prodji--docker-command (action)
  (string-join `(,prodji-docker-compose-executable ,action) " "))

(defun prodji-concat-path (&rest parts)
  "Concatenate a file path, using expand-file-name to join PARTS intelligently.

e.g. (prodji-concat-path \"/etc\" \"nginx.conf.d\") -> \"/etc/nginx.conf.d\""
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

(defun prodji-virtualenvwrapper-read-name ()
  "Get the venv name using virtualenvwrapper."
  (venv-read-name
   (if venv-current-name
       (format "Project venv (currently %s): " venv-current-name)
     "Project venv: ")))

(defun prodji-poetry-read-name ()
  "Get the venv name for a poetry-created virtual environment."
  (let ((choices (split-string
		  (shell-command-to-string
		   (concat "ls " prodji-poetry-virtual-env-dir))))
	(prompt (if prodji-venv-name
		    (format "Project venv (currently %s): " prodji-venv-name)
		  "Project venv: ")))
    (completing-read prompt choices nil t nil)))

(defun prodji-poetry-select-project ()
  (let* ((current-project (when prodji-project-root
			   (car (last (split-string prodji-project-root "/")))))
	 (prompt (if current-project
		     (format "Project (currently %s): " current-project)
		   "Project: "))
	 (project-path (or (string-trim
			    (read-file-name prompt prodji-project-directory nil t "."))
			   (user-error "prodji: must select a project to proceed"))))
    (setq prodji-project-root project-path)
    (with-temp-buffer
      (cd project-path)
      (if-let ((venv-path (shell-command-to-string "poetry env info --path")))
	  (car (last (split-string (string-trim venv-path) "/")))
	(user-error "prodji: no poetry venv found")))))

(defun prodji-poetry ()
  (interactive)
  (setq prodji-virtual-env-manager 'poetry)
  (--prodji #'prodji-poetry-select-project))

(defun prodji-virtualenvwrapper ()
  (interactive)
  (setq prodji-virtual-env-manager 'virtualenvwrapper)
  (--prodji #'prodji-virtualenvwrapper-read-name))

(defun --prodji (venv-name-read-function)
  "Start work on a Django project.

Requires a Python virtual environment created with
virtualenvwrapper, with the project root set (this is stored in
the .project file in the virtual environment directory, when the
virtual environment is created with a -a flag). If a project is
already active, stop its processes and kill their buffers."
  (interactive)
  (let* ((venv-name (funcall venv-name-read-function))
	 (kill-buffer-query-functions nil))
    (when prodji-shell-buffer
      (prodji-teardown-shell nil))
    (when prodji-server-process-buffer
      (prodji--kill-server-process))
    (prodji--setup-next-project venv-name)))

(defmacro prodji--with-server-buffer (&rest body)
  `(if prodji-server-process-buffer
       (with-current-buffer (prodji-server-buffer prodji-server-process-buffer)
	 ,@body)
     (user-error "no active server buffer")))

(defmacro prodji--match-server-type (&rest type-cases)
  `(if prodji-server-process-buffer
       (cl-case (prodji-server-type prodji-server-process-buffer)
	 ,@type-cases)
     (user-error "no active django server")))

(defun prodji--kill-server-process ()
  (prodji--match-server-type
   ('docker (prodji-teardown-docker nil))
   ('shell-process (prodji-teardown-django-server nil))))

(defun prodji--get-python-version-for-pyright ()
  "Get a MAJOR.MINOR python version string for the pyright config."
  (let ((semver-string (cadr (split-string (shell-command-to-string "python --version")))))
    (string-match "^[0-9]\\.[0-9]" semver-string)
    (match-string 0 semver-string)))

(defun prodji--maybe-write-pyright-config (project-root venv-name)
  "Write a basic pyright config json file."
  (let ((pyright-config-path (prodji-concat-path project-root "pyrightconfig.json")))
    (when (and prodji-create-pyright-config
	       (not (file-exists-p pyright-config-path)))
      (let* ((python-version (prodji--get-python-version-for-pyright))
	     (output (json-serialize
		      `(:venv ,venv-name :pythonVersion ,python-version))))
	(append-to-file output nil pyright-config-path)))))

(defun prodji--project-root-from-virtualenvwrapper ()
  (let ((project-file (expand-file-name ".project" venv-current-dir)))
    (with-temp-buffer
      (insert-file-contents project-file)
      (string-trim (buffer-string)))))

(defun prodji--project-root-from-choice ()
  (or (string-trim
       (read-file-name
	"Select project root: "
	(expand-file-name prodji-project-directory)))
      (user-error "must select project root directory")))

(defun prodji--project-root-from-var ()
  prodji-project-root)

(defun prodji--setup-next-project (venv-name)
  (prodji-activate-venv venv-name)
  (let* ((project-root-getter (if (eq prodji-virtual-env-manager 'virtualenvwrapper)
				  #'prodji--project-root-from-virtualenvwrapper
				#'prodji--project-root-from-var))
	 (project-root (funcall project-root-getter))
	 (server-buffer (when prodji-run-server
			  (prodji-server-buffer
			   (prodji-start-docker-or-django project-root)))))
    (prodji--maybe-write-pyright-config project-root venv-name)
    (prodji-start-shell-process project-root venv-name)
    (setq prodji-project-root project-root)
    (when server-buffer (pop-to-buffer server-buffer))))

(defun prodji-start-docker-or-django (project-root)
  (cond ((file-exists-p (prodji-concat-path project-root "docker-compose.yml"))
	 (let ((docker-func (if (eq prodji-virtual-env-manager 'virtualenvwrapper)
				#'prodji--virtualenvwrapper-start-docker-process
			      #'prodji--poetry-start-docker-process)))
	   (setq prodji-server-process-buffer
		 (make-prodji-server :buffer (funcall docker-func project-root)
				     :type 'docker))))
	((file-exists-p (prodji-concat-path project-root ".env"))
	 (setq prodji-server-process-buffer
	       (make-prodji-server :buffer (prodji-start-django-server project-root)
				   :type 'shell-process)))
	(t (user-error "Need docker-compose.yml or .env to run server process"))))

(defun prodji--get-venv-directory ()
  "Use this in place of `lsp-pylsp-get-pyenv-environment' with `advice-add'.

Using a named function means we can also `advice-remove' when
we're done."
  prodji-venv-directory)

(defun prodji-activate-venv (venv-name)
  (setq prodji-venv-name venv-name)
  (pcase prodji-virtual-env-manager
    ('poetry (setq prodji-venv-directory
		   (prodji-concat-path prodji-poetry-virtual-env-dir venv-name)))
    ('virtualenvwrapper (progn
			  (venv-workon venv-name)
			  (setq prodji-venv-directory venv-current-dir))))
  (setq flycheck-python-pylint-executable
	(prodji-concat-path prodji-venv-directory "bin" "python"))

  ;; lsp-mode calls `lsp-pylsp-get-pyenv-environment' on startup to
  ;; get the env which it passes to jedi, which jedi uses for lookups.
  ;; By bypassing this, we can use whatever env we've found here.
  (advice-add
   'lsp-pylsp-get-pyenv-environment
   :override
   #'prodji--get-venv-directory)

  ;; Opt-in to flake8 if setup.cfg exists in the project root
  (when-let* ((setup-cfg (prodji-concat-path prodji-project-root "setup.cfg"))
	      (exists (file-exists-p setup-cfg)))
    (setq lsp-pylsp-plugins-flake8-config setup-cfg
	  lsp-pylsp-plugins-flake8-enabled t))

  ;; Add the venv bin directory to exec-path for other libs that use
  ;; `executable-find' etc.
  (push (prodji-concat-path prodji-venv-directory "bin") exec-path)

  (when-let ((settings (prodji-dot-env-get "DJANGO_SETTINGS_MODULE")))
    (setenv "DJANGO_SETTINGS_MODULE" settings)))

(defun prodji-killall ()
  (interactive)
  (when prodji-project-root
    (let ((kill-buffer-query-functions nil))
      (when prodji-server-process-buffer
	(prodji--kill-server-process))
      (prodji-teardown-shell)
      (call-interactively 'prodji-kill-project-buffers)

      ;; Remove the venv bin directory from exec-path
      (setq exec-path (seq-filter
		       (lambda (path)
			 (not (string= path
				       (prodji-concat-path prodji-venv-directory "bin"))))
		       exec-path))
      (setq prodji-project-root nil
	    prodji-venv-name nil
	    prodji-venv-directory nil
	    flycheck-python-pylint-executable "python")
      (advice-remove 'lsp-pylsp-get-pyenv-environment #'prodji--get-venv-directory)
      (setq lsp-pylsp-plugins-flake8-config nil
	    lsp-pylsp-plugins-flake8-enabled nil)
      (when (eq prodji-virtual-env-manager 'virtualenvwrapper)
	(venv-deactivate)))))

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

(cl-defmacro prodji--teardown-process
    ((buffer &key show-progress sentinel) &rest how)
  (declare (indent defun))
  (let ((sentinel-not-nil (fboundp sentinel)))
    `(when ,buffer
       (let ,(append `((process (get-buffer-process ,buffer)))
		     (and show-progress
			  `((reporter
			     (make-progress-reporter
			      (format "Stopping %s..." ,buffer))))))
	 (if process
	     (progn
	       (when ,sentinel-not-nil
		 (set-process-sentinel process
				       (lambda (proc output)
					 (,sentinel proc output)
					 (when ,show-progress
					   (progress-reporter-done reporter)))))
	       (with-current-buffer ,buffer ,@how))
	   (kill-buffer ,buffer))
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
    (kill-buffer (current-buffer))))

(defun prodji-teardown-docker (&optional preserve-buffer)
  (let ((buf (prodji-server-buffer prodji-server-process-buffer)))
    (prodji--teardown-process
      (buf :show-progress t
	   :sentinel prodji--kill-buffer-when-finished)
      (call-process-shell-command (prodji--docker-command "stop") nil nil nil)
      (setq prodji-server-process-buffer nil))))

(defun prodji-teardown-django-server (&optional preserve-buffer)
  (let ((buf (prodji-server-buffer prodji-server-process-buffer)))
    (prodji--teardown-process
      (buf :sentinel prodji--kill-buffer-when-finished)
      (interrupt-process (get-buffer-process buf))
      (setq prodji-server-process-buffer nil))))

(defun prodji-start-shell-process (project-root venv-name)
  (with-temp-buffer
    (cd project-root)
    (let ((shell-buffer (vterm (format "*shell-%s*" venv-name))))
      (setq prodji-shell-buffer shell-buffer))))

(defun prodji--virtualenvwrapper-start-docker-process (working-directory)
  (let ((docker-buffer (get-buffer-create (format "*docker-%s*" prodji-venv-name))))
    (with-current-buffer docker-buffer
      (cd working-directory)
      (let ((args (append (split-string-shell-command prodji-docker-compose-executable)
			  '("up" "--no-color"))))
	(apply 'start-file-process
	       (format "docker-compose:%s" prodji-venv-name)
	       docker-buffer
	       args))
      (prodji-docker-mode))
    docker-buffer))

(defun prodji--poetry-start-docker-process (working-directory)
  (let ((docker-buffer (get-buffer-create (format "*docker-%s*" prodji-venv-name))))
    (with-current-buffer docker-buffer
      (cd working-directory)
      (comint-mode)
      (visual-line-mode 'disable)
      (call-process-shell-command (prodji--docker-command "up -d") nil nil nil)
      (let* ((command '("docker-compose"
			"exec"
			"--"
			"web"
			"/venv/bin/honcho"
			"start"
			"-f"
			"/app/docker/Procfile"))
	     (process
	      (start-process-shell-command
	       (format "docker-compose:%s" prodji-venv-name)
	       docker-buffer
	       (string-join command " "))))
	(set-process-filter process 'comint-output-filter)))
    docker-buffer))

(defun prodji-dot-env-get (var-name)
  "Get the value of VAR-NAME from project-root/.env or nil."
  (let ((dot-env-file (prodji-concat-path prodji-project-root ".env")))
    (if (file-readable-p dot-env-file)
	(with-temp-buffer
	  (insert-file-contents dot-env-file)
	  (goto-char (point-min))
	  (if (search-forward-regexp (concat "^" var-name "="))
	      (buffer-substring-no-properties (point) (line-end-position))
	    nil))
      nil)))

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
    server-buffer))

(defun prodji-restart-server ()
  (interactive)
  (prodji--match-server-type
   ('docker (prodji-restart-docker))
   ('shell-process (prodji-restart-django-server))))

(defun prodji-restart-django-server ()
  (prodji-teardown-django-server t)
  (pop-to-buffer-same-window
   (prodji-start-docker-or-django prodji-project-root)))

(defun prodji-restart-docker ()
  (prodji-teardown-docker t)
  (pop-to-buffer-same-window
   (prodji-start-docker-or-django prodji-project-root)))

(defun prodji--get-management-command-prefix ()
  (or (and (eq (prodji-server-type prodji-server-process-buffer) 'docker)
	   (append (split-string-shell-command prodji-docker-compose-executable)
		   '("run" "--rm" "web")))
      '()))

(defun prodji-find-management-commands-from-dir (dir)
  "Find Django management commands, searching from DIR.

Returns names of .py files in **/management/commands/."
  (let* ((find-query
	  "-path '*/management/commands/*.py' -not -name '__init__.py' -exec basename {} .py \\;")
	 (command (string-join `("find" ,dir ,find-query) " ")))
    (with-temp-buffer
      (shell-command command t)
      (split-string (buffer-string)))))

(defun prodji-run-django-command ()
  (interactive)
  (when (not prodji-shell-buffer)
    (user-error "project not activated"))
  (let* ((command (completing-read
		   "Command: "
		   (append
		    (prodji-find-management-commands-from-dir prodji-venv-directory)
		    (prodji-find-management-commands-from-dir prodji-project-root))))
	 (buf (get-buffer-create "*prodji-management-command*"))
	 (program (append (prodji--get-management-command-prefix)
			  '("python" "manage.py")
			  (split-string-shell-command command))))
    (with-current-buffer buf
      (cd prodji-project-root)
      (delete-region (point-min) (point-max))
      (pop-to-buffer buf)
      (apply 'make-comint-in-buffer
	     (format "<prodji:%s>" command)
	     buf
	     (car program)
	     nil
	     (cdr program)))))

(defun prodji-goto-server (prefix)
  (interactive "P")
  (prodji--with-server-buffer
   (prodji--goto-buffer (current-buffer) prefix)))

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
