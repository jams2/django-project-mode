(defvar prodji-current-docker nil)
(defvar prodji-current-shell nil)
(defvar prodji-project-root nil)

(defun concat-path (&rest parts)
  "Concatenate a file path, using expand-file-name to join PARTS intelligently.

e.g. (concat-path \"/etc\" \"nginx.conf.d\") -> \"/etc/nginx.conf.d\""
  (reduce (lambda (a b) (expand-file-name b a)) parts))

(defun prodji (preserve-buffers)
  "Start working on a Python project. With prefix arg, preserve existing shell processes."
  (interactive "P")
  (let ((venv-name (venv-read-name
		    (if venv-current-name
			(format "Project venv (currently %s): " venv-current-name)
		      "Project venv: ")))
	(kill-buffer-query-functions nil))
    (prodji-teardown-docker preserve-buffers)
    (prodji-teardown-shell preserve-buffers)
    (setq prodji-project-root nil)
    (venv-workon venv-name))
  (let* ((shell-buffer (prodji-start-shell-process))
	 (project-file (expand-file-name ".project" venv-current-dir))
	 (project-root (with-temp-buffer
			 (insert-file-contents project-file)
			 (string-trim (buffer-string))))
	 (docker-buffer (and (file-exists-p
			      (concat-path project-root
					   "docker-compose.yml"))
			     (prodji-start-docker-shell project-root))))
    (setq prodji-project-root project-root)
    (if docker-buffer (switch-to-buffer docker-buffer)
      (switch-to-buffer shell-buffer))))

(defun prodji-killall (preserve-buffers)
  (interactive "P")
  (when prodji-project-root
    (let ((kill-buffer-query-functions nil))
      (prodji-teardown-docker preserve-buffers)
      (prodji-teardown-shell preserve-buffers)
      (setq prodji-project-root nil)
      (venv-deactivate))))

(defun prodji-teardown-shell (preserve-buffer)
  (when prodji-current-shell
      (with-current-buffer prodji-current-shell
	(comint-interrupt-subjob))
      (unless preserve-buffer
	(interrupt-process (get-buffer-process prodji-current-shell))
	(kill-buffer prodji-current-shell))
      (setq prodji-current-shell nil)))

(defun prodji-teardown-docker (preserve-buffer)
  (when prodji-current-docker
    (let ((progress-reporter (make-progress-reporter "Stopping docker...")))
      (with-current-buffer prodji-current-docker
	;; stop the already running docker image
	(call-process-shell-command "docker-compose stop" nil nil nil))
      (unless preserve-buffer
	(kill-process (get-buffer-process prodji-current-docker))
	(kill-buffer prodji-current-docker))
      (setq prodji-current-docker nil))))

(defun prodji-start-shell-process ()
  (let ((shell-buffer (shell (format "*shell-%s*" venv-current-name))))
    (setq prodji-current-shell shell-buffer)))

(defun prodji-start-docker-shell (working-directory)
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
    (setq prodji-current-docker docker-buffer)))

(defun prodji--get-management-command-prefix ()
  (or (and prodji-current-docker "docker-compose run web")
      ""))

(defun prodji-run-django-command ()
  (interactive)
  (when (not prodji-current-shell)
    (user-error "project not activated"))
  (let ((command (completing-read
		  "Command: "
		  '("makemigrations" "collectstatic" "migrate" "showmigrations" "shell"))))
    (pop-to-buffer prodji-current-shell)
    (comint-send-string
     (get-buffer-process prodji-current-shell)
     (format
      "%s python manage.py %s\n"
      (prodji--get-management-command-prefix)
      command))))

(defun prodji-goto-docker (prefix)
  (interactive "p")
  (prodji--goto-buffer prodji-current-docker (not (equal prefix 1))))

(defun prodji-goto-shell (prefix)
  (interactive "p")
  (prodji--goto-buffer prodji-current-shell (not (equal prefix 1))))

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
