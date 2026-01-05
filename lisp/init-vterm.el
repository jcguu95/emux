;;; -*- lexical-binding: t; -*-

(unless (locate-library "vterm")
  ;; Ask elpaca to manage vterm only if the system does not already.
  ;; This is useful for guix.
  (use-package vterm :ensure t))

(use-package vterm
  :defer t
  :preface
  
  (with-eval-after-load 'init-bindings
    (my-leader
     "t" '(:ignore t :which-key "Terminal")
     "tt" '(my/run-named-vterm :which-key "New Vterm")))
  
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


;;; Config
  :config

;;; RUN-IN-VTERM
  (cl-defun run-in-vterm (&key
                          buffer-name
                          (command "")
                          cancel-if-buffer-exists
                          double-check)
    "Execute COMMAND in a vterm buffer named BUFFER-NAME.

If BUFFER-NAME is nil or an empty string, the user is prompted for a
name via the minibuffer.

If a buffer with the provided name already exists and is in `vterm-mode`,
the function switches to that buffer and executes the command there.
Otherwise, it creates a new vterm session with that name.

COMMAND is sent to the terminal followed by a return key sequence to 
trigger execution.

If CANCEL-IF-BUFFER-EXISTS is non-nil and a buffer with BUFFER-NAME
already exists, the function will message the user and exit without
running COMMAND.

If DOUBLE-CHECK is non-nil, prompt the user for confirmation before
executing the command.
"
    ;; For example, try
    ;; (progn
    ;;   (run-in-vterm :buffer-name "vterm: test"
    ;;                 :command "echo 1 2 3 4; sleep 1")
    ;;   (run-in-vterm :buffer-name "vterm: test"
    ;;                 :command "echo 1 2 3  ; sleep 1")
    ;;   (run-in-vterm :buffer-name "vterm: test"
    ;;                 :command "echo 1 2    ; sleep 1"
    ;;                 :double-check t)
    ;;   (run-in-vterm :buffer-name "vterm: test"
    ;;                 :command "echo 1      ;"
    ;;                 :cancel-if-buffer-exists t))
    (interactive
     (list (read-string "Command: ")
           :buffer-name (read-string "Vterm buffer name: ")
           :cancel-if-buffer-exists nil
           :double-check nil))
    (let* ((name (if (or (null buffer-name) (string-empty-p buffer-name))
                     (read-string "Enter vterm buffer name: ")
                   buffer-name))
           (buf (get-buffer name)))
      (cond
       ;; Case 1: Buffer exists and we are told to cancel
       ((and buf cancel-if-buffer-exists)
        (message "Action cancelled: vterm buffer '%s' already exists.\nCommand: %s\n" name command))
       
       ;; Case 2: Double-check is requested and user says no
       ((and double-check 
             (not (y-or-n-p (format "Are you sure you want to run '%s'? " command))))
        (message "Command aborted."))

       ;; Case 3: Run the command
       (t
        ;; If buffer doesn't exist or isn't a vterm buffer, create it silently
        (unless (and buf (with-current-buffer buf (derived-mode-p 'vterm-mode)))
          (setq buf (generate-new-buffer name))
          (with-current-buffer buf
            (vterm-mode)))
        ;; Send command to the buffer in the background
        (message "running command in buffer %s" (buffer-name buf))
        (with-current-buffer buf
          (vterm-send-string command)
          (vterm-send-return))
        ;; Return the buffer object
        buf)
       )))
  
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

  (with-eval-after-load 'init-bindings
    (my-local-leader
     "e" '(my/vterm-edit-command :which-key "vterm edit")))
     )

(provide 'init-vterm)
