;;; -*- lexical-binding: t; -*-

;;; Core
;;; Designed for fast booting in terminal mode.

(require 'init-ui)
(require 'init-evil)
(require 'init-terminal)                     
(require 'init-bindings)
(require 'init-window)
(require 'init-completion)

;; TODO --- System Clipboard ---
;; Integrate `emacs -nw` and system clipboard (#'evil-yank and
;; #'evil-paste-after). See how doomemacs did this.

(provide 'init-core-profile)
