(defvar workon-current-docker nil)
(defvar workon-current-shell nil)
(defvar workon-project-root nil)

(defun concat-path (&rest parts)
  "Concatenate a file path, using expand-file-name to join PARTS intelligently.

e.g. (concat-path \"/etc\" \"nginx.conf.d\") -> \"/etc/nginx.conf.d\""
  (reduce (lambda (a b) (expand-file-name b a)) parts))

(defun workon (preserve-buffers)
  "Start working on a Python project. With prefix arg, preserve existing shell processes."
  (interactive "P")
  (let ((venv-name (venv-read-name
		    (if venv-current-name
			(format "Project venv (currently %s): " venv-current-name)
		      "Project venv: ")))
	(kill-buffer-query-functions nil))
    (workon-teardown-docker preserve-buffers)
    (workon-teardown-shell preserve-buffers)
    (setq workon-project-root nil)
    (venv-workon venv-name))
  (let* ((shell-buffer (workon-start-shell-process))
	 (project-file (expand-file-name ".project" venv-current-dir))
	 (project-root (with-temp-buffer
			 (insert-file-contents project-file)
			 (string-trim (buffer-string))))
	 (docker-buffer (and (file-exists-p
			      (concat-path project-root
					   "docker-compose.yml"))
			     (workon-start-docker-shell project-root))))
    (setq workon-project-root project-root)
    (if docker-buffer (switch-to-buffer docker-buffer)
      (switch-to-buffer shell-buffer))))

(defun workon-killall (preserve-buffers)
  (interactive "P")
  (when workon-project-root
    (let ((kill-buffer-query-functions nil))
      (workon-teardown-docker preserve-buffers)
      (workon-teardown-shell preserve-buffers)
      (setq workon-project-root nil)
      (venv-deactivate))))

(defun workon-teardown-shell (preserve-buffer)
  (when workon-current-shell
      (with-current-buffer workon-current-shell
	(comint-interrupt-subjob))
      (unless preserve-buffer
	(interrupt-process (get-buffer-process workon-current-shell))
	(kill-buffer workon-current-shell))
      (setq workon-current-shell nil)))

(defun workon-teardown-docker (preserve-buffer)
  (when workon-current-docker
    (let ((progress-reporter (make-progress-reporter "Stopping docker...")))
      (with-current-buffer workon-current-docker
	;; stop the already running docker image
	(call-process-shell-command "docker-compose stop" nil nil nil))
      (unless preserve-buffer
	(kill-process (get-buffer-process workon-current-docker))
	(kill-buffer workon-current-docker))
      (setq workon-current-docker nil))))

(defun workon-start-shell-process ()
  (let ((shell-buffer (shell (format "*shell-%s*" venv-current-name))))
    (setq workon-current-shell shell-buffer)))

(defun workon-start-docker-shell (working-directory)
  (let ((docker-buffer (get-buffer-create (format "*docker-%s*" venv-current-name))))
    (with-current-buffer docker-buffer
      (cd working-directory)
      (start-file-process
       (format "docker-compose:%s" venv-current-name)
       docker-buffer
       "docker-compose"
       "up"
       "--no-color")
      (special-mode))
    (setq workon-current-docker docker-buffer)))

(defun workon-run-django-command ()
  (interactive)
  (when (not workon-current-shell)
    (user-error "project not activated"))
  (let ((command (completing-read
		  "Command: "
		  '("makemigrations" "collectstatic" "migrate" "showmigrations" "shell"))))
    (pop-to-buffer workon-current-shell)
    (comint-send-string
     (get-buffer-process workon-current-shell)
     (format "docker-compose run web python manage.py %s\n" command))))

(defun workon-goto-docker (prefix)
  (interactive "p")
  (workon--goto-buffer workon-current-docker (not (equal prefix 1))))

(defun workon-goto-shell (prefix)
  (interactive "p")
  (workon--goto-buffer workon-current-shell (not (equal prefix 1))))

(defun workon--goto-buffer (buffer in-other-window)
  (cond ((not buffer) (user-error "project not activated"))
	(in-other-window (switch-to-buffer-other-window buffer))
	(t (switch-to-buffer buffer))))
