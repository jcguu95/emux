;;; -*- lexical-binding: t; -*-

(use-package eat
  ;; NOTE A better terminal may be vterm with indirect-edit enhancement,
  ;; because its line-mode is worse than EAT's line-mode.
  :defer t
  :config
  (add-hook 'eat-exec-hook (lambda (x)
                             (declare (ignore x))
                             (eat-line-mode))))

(provide 'init-eat)
