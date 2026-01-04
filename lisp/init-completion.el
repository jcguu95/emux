;;; -*- lexical-binding: t; -*-

;; 簡潔的完成介面
(use-package vertico
  :ensure t
  :defer t
  :init (vertico-mode))

;; Orderless - 強大的模糊匹配
(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  ;; (orderless-mode) ; FIXME not present
  ) 

;; Marginalia - 為 Vertico 候選列表添加額外資訊
(use-package marginalia
  :ensure t
  :defer t
  :after vertico
  :init
  (marginalia-mode))

;; Consult - 多功能互動式命令
(use-package consult
  :ensure t
  :defer t
  :after (vertico orderless) ;; 確保在 vertico 和 orderless 之後加載
  :config
  ;; 可以在這裡定義 consult 相關的鍵綁定，如果 general.el 中未定義
  ;; 確保 consult 的命令被 general.el 拾取或手動綁定
  ;; 例如：(global-set-key (kbd "C-c C-r") 'consult-recent-files)
  )

(provide 'init-completion)
