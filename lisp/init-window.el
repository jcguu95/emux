;;; -*- lexical-binding: t; -*-

(use-package winner
  :defer t
  :commands (winner-undo winner-redo)
  :config
  (defun my/one-shot-winner-mode-activator ()
    "啟用 winner-mode，然後移除此 advice 和函數定義本身。"
    (unless winner-mode
      (winner-mode 1)
      (advice-remove 'winner-undo #'my/one-shot-winner-mode-activator)
      (advice-remove 'winner-redo #'my/one-shot-winner-mode-activator)
      (fmakunbound 'my/one-shot-winner-mode-activator)))
  (advice-add 'winner-undo :before #'my/one-shot-winner-mode-activator)
  (advice-add 'winner-redo :before #'my/one-shot-winner-mode-activator))

(provide 'init-window)
