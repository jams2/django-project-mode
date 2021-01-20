(defvar prodji-docker-buffer nil)
(defvar prodji-shell-buffer nil)
(defvar prodji-project-root nil)
(defvar prodji-django-server-buffer nil)

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
    (prodji-teardown-django-server preserve-buffers)
    (setq prodji-project-root nil)
    (venv-workon venv-name))
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

(defun prodji-killall (preserve-buffers)
  (interactive "P")
  (when prodji-project-root
    (let ((kill-buffer-query-functions nil))
      (prodji-teardown-server preserve-buffers)
      (prodji-teardown-shell preserve-buffers)
      (setq prodji-project-root nil)
      (venv-deactivate))))

(defun prodji-teardown-server (preserve-buffer)
  (cond (prodji-docker-buffer (prodji-teardown-docker preserve-buffer))
	(prodji-django-server-buffer (prodji-teardown-django-server
				      preserve-buffer))
	(t (user-error "No active prodji server process"))))

(defun prodji-teardown-shell (preserve-buffer)
  (when prodji-shell-buffer
      (with-current-buffer prodji-shell-buffer
	(comint-interrupt-subjob))
      (unless preserve-buffer
	(interrupt-process (get-buffer-process prodji-shell-buffer))
	(kill-buffer prodji-shell-buffer))
      (setq prodji-shell-buffer nil)))

(defun prodji-teardown-docker (preserve-buffer)
  (when prodji-docker-buffer
    (let ((progress-reporter (make-progress-reporter "Stopping docker...")))
      (with-current-buffer prodji-docker-buffer
	;; stop the already running docker image
	(call-process-shell-command "docker-compose stop" nil nil nil)
	(progress-reporter-done progress-reporter))
      (unless preserve-buffer
	(kill-process (get-buffer-process prodji-docker-buffer))
	(kill-buffer prodji-docker-buffer))
      (setq prodji-docker-buffer nil))))

(defun prodji--django-process-sentinel (proc output)
  (when (string= output "finished\n")
    (kill-buffer (process-buffer proc))))

(defun prodji-teardown-django-server (preserve-buffer)
  (when prodji-django-server-buffer
    (when (not preserve-buffer)
      (set-process-sentinel
       (get-buffer-process prodji-django-server-buffer)
       'prodji--django-process-sentinel))
    (interrupt-process (get-buffer-process prodji-django-server-buffer))
    (setq prodji-django-server-buffer nil)))

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
      (error "Can't read .env: %s" (concat-path prodji-project-root ".env")))
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
    (prodji-teardown-django-server nil))
  (pop-to-buffer-same-window
   (prodji-start-django-server prodji-project-root)))

(defun prodji-restart-docker ()
  (when prodji-docker-buffer
    (prodji-teardown-docker nil))
  (pop-to-buffer-same-window
   (prodji-start-docker-process prodji-project-root)))

(defun prodji--get-management-command-prefix ()
  (or (and prodji-docker-buffer "docker-compose run web")
      ""))

(defun prodji-run-django-command ()
  (interactive)
  (when (not prodji-shell-buffer)
    (user-error "project not activated"))
  (let ((command (completing-read
		  "Command: "
		  '("makemigrations" "collectstatic" "migrate" "showmigrations" "shell"))))
    (pop-to-buffer prodji-shell-buffer)
    (comint-send-string
     (get-buffer-process prodji-shell-buffer)
     (format
      "%s python manage.py %s\n"
      (prodji--get-management-command-prefix)
      command))))

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
