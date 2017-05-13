;;; projectile-django.el --- Django integration for projectile.
;;; Commentary:
;;; Code:

(defvar projectile-django-serve-for-everyone nil
  "Set to t if you want to bind the server to 0.0.0.0.")

(defvar projectile-django-default-port 8000
  "Default port to bind the django server.")

(defvar projectile-django-server-command "runserver"
  "Command to run the django server.")

(defvar projectile-django-python-interpreter "python"
  "Path variable to the python interpreter.

Switch to a full route if you want to get it out of an environment.")

(defvar projectile-django-default-quit-action 'bury
  "Action to take on quitting projectile-django buffers.

Default is 'bury, which means bury the buffer, you can also set
it to 'kill, which would mean kill the buffer instead.

Any other symbol will default to 'bury.")


;; Require our libraries
(require 'comint)
(require 'projectile)

;; General purpose defs
(defun projectile-django--locate-manage ()
  "Return the full path of manage.py in the project."
  (expand-file-name (concat (projectile-project-root)
                            "manage.py")))

(defun projectile-django-quit-action ()
  "Quit the current buffer.

The action taken is defined in `projectile-django-default-quit-action'."
  (interactive)
  (if (eq 'quit projectile-django-default-quit-action)
      (kill-this-buffer)
    (bury-buffer)))

;; Server specific defs
(defvar projectile-django-server-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-q") 'bury-buffer)
    (define-key map (kbd "C-c C-r") 'projectile-django-restart-server)
    (define-key map (kbd "C-c C-k") 'projectile-django-terminate-server)
    map)
  "Mode map for `projectile-django-server-mode'.")

(define-derived-mode projectile-django-server-mode comint-mode "django-server"
  "Major mode for interacting with a django server used by `projectile-django'.

Killing the buffer will terminate its server process.

\\{projectile-django-server-mode-map}"
  (add-hook 'kill-buffer-hook 'projectile-django--kill-server nil t)
  (add-hook 'kill-emacs-hook 'projectile-django--kill-server nil t))

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

(defun projectile-django--set-up-server-buffer (buffer)
  "Set up the BUFFER for the django server."
  (save-current-buffer
    (set-buffer buffer)
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (projectile-django-server-mode)))

(defun projectile-django-server ()
  "Run the django server if it's not running, otherwise switch to its buffer."
  (interactive)
  (let* ((server-buffer-name (projectile-django--get-server-buffer-name))
         (process (get-buffer-process server-buffer-name)))
    (when (member server-buffer-name (mapcar 'buffer-name (buffer-list)))
      (switch-to-buffer server-buffer-name))
    (when (not process)
      (let ((server-buffer
             (apply 'make-comint-in-buffer
                    (concat (projectile-project-name) "-django-server")
                    (projectile-django--get-server-buffer-name)
                    projectile-django-python-interpreter
                    nil
                    (split-string-and-unquote (projectile-django--assemble-server-command)))))
        (projectile-django--set-up-server-buffer server-buffer)))))

(defun projectile-django--kill-server ()
  "Kill the current django server."
  (let* ((server-buffer (get-buffer (projectile-django--get-server-buffer-name)))
         (process (get-buffer-process server-buffer)))
    (when process
      (delete-process process))))

(defun projectile-django-terminate-server ()
  "Terminate the django server process and kill its buffer."
  (interactive)
  (let ((server-buffer (get-buffer (projectile-django--get-server-buffer-name))))
    (projectile-django--kill-server)
    (kill-buffer server-buffer)))

(defun projectile-django-restart-server ()
  "Restart the django server."
  (interactive)
  (projectile-django--kill-server)
  (let ((server-buffer (get-buffer (projectile-django--get-server-buffer-name))))
    (save-current-buffer
      (set-buffer server-buffer)
      (projectile-django-server))))


;; Migration defs
(defvar projectile-django-migration-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (suppress-keymap map)
    (define-key map (kbd "q") 'projectile-django-quit-action)
    map)
  "Mode map for `projectile-django-migration-mode'.")

(define-derived-mode projectile-django-migration-mode comint-mode "django-migrate"
  "Major mode for django migrations used by `projectile-django'.

Quitting the buffer will trigger `projectile-django-quit-action'.

\\{projectile-django-migration-mode-map}"
  (setq buffer-read-only t))

(defun projectile-django--assemble-migrate-all-command ()
  "Return a string with the migrate command."
  (format "%s %s"
          (projectile-django--locate-manage)
          "migrate"))

(defun projectile-django--get-migrate-buffer-name ()
  "Return a suitable buffer name for the migration buffer."
  (concat "*" (projectile-project-name) "-migrations*"))

(defun projectile-django--set-up-migration-buffer (buffer)
  "Set up the migration buffer BUFFER for django."
  (save-current-buffer
    (set-buffer buffer)
    (let ((process (get-buffer-process buffer)))
      (when process
        (delete-process process)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)))

(defun projectile-django-migrate-all ()
  "Migrate all apps in project."
  (interactive)
  (let* ((migration-buffer (projectile-django--get-migrate-buffer-name))
         (process (get-buffer-process migration-buffer)))
    (when (member migration-buffer (mapcar 'buffer-name (buffer-list)))
      (projectile-django--set-up-migration-buffer migration-buffer)
      (switch-to-buffer migration-buffer))
    (when (not process)
      (let ((migration-buffer
             (apply 'make-comint-in-buffer
                    (concat (projectile-project-name) "-migrations")
                    (projectile-django--get-migrate-buffer-name)
                    projectile-django-python-interpreter
                    nil
                    (split-string-and-unquote (projectile-django--assemble-migrate-all-command)))))
        (save-current-buffer
          (set-buffer migration-buffer)
          (switch-to-buffer migration-buffer)
          (projectile-django-migration-mode))))))


(provide 'projectile-django)
;;; projectile-django.el ends here
