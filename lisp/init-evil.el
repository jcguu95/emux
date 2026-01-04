;;; -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ; 不自動綁定鍵，交由 evil-collection 和 general.el 處理
  (setq evil-undo-system 'undo-redo)    ; Emacs 28+ 內建撤銷
  :config
  (evil-mode 1)
  (setq evil-esc-delay 0)) ; 終端 ESC 不延遲

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(provide 'init-evil)
