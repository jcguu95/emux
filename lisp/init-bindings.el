;;; -*- lexical-binding: t; -*-

(use-package general
  :defer nil
  :config
  
  (general-create-definer my-leader
    :states '(normal visual emacs)
    :prefix "SPC")
  
  (general-create-definer my-local-leader
    :states '(normal visual emacs)
    :prefix "SPC m"                     ; Doom style
    :non-normal-prefix "M-SPC m"
    :keymaps 'override)
  
  (my-leader
   ":" '(execute-extended-command :which-key "M-x") ; FIXME :which-key didn't seem to work here
   "x" '(my/toggle-scratch-buffer :which-key "toggle scratch buffer")
   "<" '(consult-buffer :which-key "switch buffer")

   ;; Buffer
   "b" '(:ignore t :which-key "Buffer")
   "bb" '(switch-to-buffer :which-key "Switch")
   "bk" '(kill-current-buffer :which-key "Kill")
   "bi" '(ibuffer :which-key "ibuffer")

   ;; Window
   "w" '(:ignore t :which-key "Window")
   "wv" '(split-window-right :which-key "Vertical split")
   "ws" '(split-window-below :which-key "Horizontal split")
   "wh" '(windmove-left :which-key "Focus Left")
   "wl" '(windmove-right :which-key "Focus Right")
   "wj" '(windmove-down :which-key "Focus Down")
   "wk" '(windmove-up :which-key "Focus Up")
   "wd" '(delete-window :which-key "Delete window")
   "wu" '(winner-undo :which-key "Undo window")
   "w C-r" '(winner-redo :which-key "Redo window")

   ;; Helpful
   "h" '(:ignore t :which-key "Helpful")
   "hf" '(helpful-callable :which-key "describe-function")
   "hv" '(helpful-variable :which-key "describe-variable")
   "hk" '(helpful-key :which-key "describe-key")
   ))

(use-package helpful
  :defer t)

(provide 'init-bindings)
