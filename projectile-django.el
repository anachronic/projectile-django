;;; projectile-django.el --- Django integration for projectile.
;;; Commentary:
;;; Code:

(defvar projectile-django-server-buffer-name "*projectile-django-server*"
  "Name of the server buffer for projectile-django.")

(defvar projectile-django-serve-for-everyone nil
  "Set to t if you want to bind the server to 0.0.0.0.")

(defvar projectile-django-default-port 8000
  "Default port to bind the django server.")

(defvar projectile-django-server-command "runserver"
  "Command to run the django server.")

(defvar projectile-django-python-interpreter "python"
  "Path variable to the python interpreter.

Switch to a full route if you want to get it out of an environment.")

(define-derived-mode projectile-django-server-mode comint-mode "django-server"
  "Compilation mode for running django server used by `projectile-django'.

Killing the buffer will terminate its server process."
  ;; (when projectile-django-server-mode-ansi-colors
  ;; (add-hook 'compilation-filter-hook 'projectile-django-apply-ansi-color nil t))
  (setq-local compilation-scroll-output t)
  (add-hook 'kill-buffer-hook 'projectile-django--kill-server t t)
  (add-hook 'kill-emacs-hook 'projectile-django--kill-server t t))

(defun projectile-django--locate-manage ()
  "Return the full path of manage.py in the project."
  (expand-file-name (concat (projectile-project-root)
                            "manage.py")))

(defun projectile-django--get-bind-string ()
  "Get bind string for the server."
  (if projectile-django-serve-for-everyone
      (format "0.0.0.0:%d" projectile-django-default-port)
    (format "%d" projectile-django-default-port)))

(defun projectile-django--assemble-server-command ()
  "Return the command string after manage.py to run the server."
  (let ((bind-string (projectile-django--get-bind-string)))
    (format "%s %s %s"
            (projectile-django--locate-manage)
            projectile-django-server-command
            bind-string)))

(defun projectile-django--get-server-buffer-name ()
  "Return a suitable buffer name for the django server."
  (concat "*" (projectile-project-name) "-django-server*"))

(defun projectile-django-server ()
  "Run the django server if it's not running, otherwise switch to its buffer."
  (interactive)
  (let ((server-buffer-name (projectile-django--get-server-buffer-name)))
    (if (member server-buffer-name (mapcar 'buffer-name (buffer-list)))
        (switch-to-buffer server-buffer-name)
      (set-buffer (apply 'make-comint-in-buffer
                         (concat (projectile-project-name) "-django-server")
                         (projectile-django--get-server-buffer-name)
                         projectile-django-python-interpreter
                         nil
                         (split-string-and-unquote (projectile-django--assemble-server-command))))
      (switch-to-buffer (current-buffer)))))

(defun projectile-django--kill-server ()
  "Kill the current django server."
  (let ((process (get-buffer-process projectile-django-server-buffer-name)))
    (when process (signal-process process 15))))

(provide 'projectile-django)
;;; projectile-django.el ends here
