;;; -*- lexical-binding: t; -*-

(unless (locate-library "vterm")
  ;; Ask elpaca to manage vterm only if the system does not already.
  ;; This is useful for guix.
  (use-package vterm :ensure t))

(use-package vterm
  :defer t
  :preface
  (defun my/run-named-vterm (name)
    "Create or switch to a vterm buffer with NAME.
If NAME is taken, append a suffix to ensure uniqueness."
    (interactive "sVterm Name: ")
    (require 'vterm)
    (let* ((unique-name (format "vterm: %s" (generate-new-buffer-name name)))
           (vterm-buffer (get-buffer-create unique-name)))
      (with-current-buffer vterm-buffer
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode)))
      (switch-to-buffer vterm-buffer)))
  
  :config
  (with-eval-after-load 'init-bindings
    (my-leader
     "t" '(:ignore t :which-key "Terminal")
     "tt" '(my/run-named-vterm :which-key "New Vterm")))
  
;;; Vterm with Command Edition (edit-indirect like)
;;; TODO We may rewrite this with edit-indirect.el.

  (defvar-local my/vterm-target-buffer nil)

  (defun my/vterm-edit-command ()
    "Compose a command for the current vterm buffer in a separate edit buffer."
    (interactive)
    (unless (derived-mode-p 'vterm-mode)
      (user-error "You must be in a vterm buffer to use this"))
    
    (let* ((origin-vterm (current-buffer))
           (edit-buffer (get-buffer-create (format "*vterm-edit: %s*" (buffer-name)))))
      (with-current-buffer edit-buffer
        (sh-mode)
        (erase-buffer) ;; Clear old content if the buffer existed
        
        ;; 1. Insert the guide comment
        (let ((start (point)))
          (insert "# Edit your vterm command below.\n#   C-c C-c to SEND,\n#   C-c C-k to ABORT.\n\n")
          ;; Manually apply the gray comment face to the text we just inserted
          (add-text-properties start (point) 
                               '(font-lock-face font-lock-comment-face
                                 rear-nonsticky (font-lock-face)))
          )
        
        (setq-local my/vterm-target-buffer origin-vterm)
        (local-set-key (kbd "C-c C-c") #'my/vterm-send-and-exit)
        (local-set-key (kbd "C-c C-k") (lambda () (interactive) (kill-buffer))))
      
      (let ((upper-win (split-window (selected-window) nil 'above)))
        (set-window-buffer upper-win edit-buffer)
        (select-window upper-win)
        ;; Place cursor at the end of the buffer (after the comment)
        (goto-char (point-max)))))

  (defun my/vterm-send-and-exit ()
    "Send buffer content to vterm, ignoring the first line comment."
    (interactive)
    (let ((cmd (save-restriction
                 ;; Ignore the first three lines (the gray comment)
                 (goto-char (point-min))
                 (forward-line 3)
                 (buffer-substring-no-properties (point) (point-max))))
          (target my/vterm-target-buffer))
      (if (buffer-live-p target)
          (progn
            (save-window-excursion      ; This "freezes" the layout
              (let ((popup-buf (current-buffer)))
                ;; Send the command
                (with-current-buffer target
                  (vterm-send-string (string-trim cmd))
                  (vterm-send-return))
                ;; Kill the popup buffer while we're inside the excursion
                (kill-buffer popup-buf)))
            (delete-window))
        (error "Target vterm buffer is gone!"))))

  (progn
    (require 'init-bindings)
    (my-local-leader
     "e" '(my/vterm-edit-command :which-key "vterm edit"))))

(provide 'init-vterm)
