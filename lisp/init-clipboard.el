;;; -*- lexical-binding: t; -*-

(use-package clipetty
  :ensure t
  ;; FIXME This does not work properly on macOS.
  :hook (after-init . global-clipetty-mode))

(unless (display-graphic-p)
  (let ((paste-cmd (cond
                    ((executable-find "pbpaste") "pbpaste")   ;; macOS
                    ((executable-find "wl-paste") "wl-paste") ;; Wayland
                    ((executable-find "xclip") "xclip -o -sel clip") ;; X11
                    ((executable-find "xsel") "xsel -ob")))) ;; X11 Alt
    (when paste-cmd
      ;; This function tells Emacs how to 'pull' from the system
      (setq interprogram-paste-function
            (lambda ()
              (let ((text (shell-command-to-string paste-cmd)))
                ;; Only return the text if it's different from the last kill
                ;; to avoid infinite loops in some edge cases
                (unless (string= text (car kill-ring))
                  text)))))))

(provide 'init-clipboard)
