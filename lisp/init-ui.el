;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)

(use-package which-key
  :defer nil
  :config
  (setq which-key-min-display-lines 1)
  (which-key-mode))

(use-package gruvbox-theme
  :ensure t
  :defer nil
  :config
  (load-theme 'gruvbox-dark-hard t)) ; Or 'gruvbox-dark-medium', 'gruvbox-light-hard', etc.

(unless (display-graphic-p)
  ;; 透明背景，跟隨終端
  (set-face-attribute 'default nil :background "unspecified-bg"))

;;;###autoload
(defun my/toggle-scratch-buffer (&optional same-window-p)
  "切換一個持久性的 *scratch* 緩衝區。
如果 *scratch* 緩衝區可見，則關閉其視窗。否則，打開它。
如果 SAME-WINDOW-P 為非 nil，則在當前視窗中打開它，否則彈出到新視窗。"
  (interactive "P") ;; "P" 傳遞原始前綴引數，這裡為了兼容性保留，但此簡化版並未使用它。
  (let* ((scratch-buffer (get-buffer-create "*scratch*"))
         (scratch-window (get-buffer-window scratch-buffer)))
    (if scratch-window
        ;; 如果 *scratch* 緩衝區可見，則關閉其視窗
        (delete-window scratch-window)
      ;; 如果不可見，則根據 same-window-p 的值顯示它
      (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer) scratch-buffer))))

(provide 'init-ui)
