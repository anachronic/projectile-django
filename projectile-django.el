;;; projectile-django.el --- Django integration for projectile.
;;; Commentary:
;;; Code:

(defvar projectile-django-server-buffer-name "*projectile-django-server*"
  "Name of the server buffer for projectile-django.")

(defvar projectile-django-server-mode-ansi-colors t
  "Set to t if you want to apply filter color on projectile django server mode.")

(define-derived-mode projectile-django-server-mode compilation-mode "django-server"
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

(defun projectile-django-server ()
  "Run the django server if it's not running, otherwise switch to its buffer."
  (interactive)
  (if (member projectile-django-server-buffer-name (mapcar 'buffer-name (buffer-list)))
      (switch-to-buffer projectile-django-server-buffer-name)
    (compile (concat "python "
                     (projectile-django--locate-manage)
                     " runserver")
             'projectile-django-server-mode)))

(defun projectile-django--kill-server ()
  "Kill the current django server."
  (let ((process (get-buffer-process projectile-django-server-buffer-name)))
    (when process (signal-process process 15))))

(provide 'projectile-django)
;;; projectile-django.el ends here
