(setq warning-minimum-level :debug
      debug-on-error t)
(setq gc-cons-threshold (* 50 1024 1024)) ; TODO may have to go into early-init.el?
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :background "unspecified-bg") ; 透明背景，跟隨終端

;; Straight.el
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

;; --- System Clipboard ---
;; 
;; TODO Integrate `emacs -nw` and system clipboard (#'evil-yank and
;; #'evil-paste-after). See how doomemacs did this.

;; NOTE 平常沒必要時，把這個刪掉以增加 booting 效率。
(use-package benchmark-init
  ;; 啟動完成後，執行 =M-x benchmark-init/show-durations= 或 =M-x
  ;; benchmark-init/show-durations-tree= 以查看詳細的啟動時間報告。報告會顯示
  ;; 每個載入項目的耗時。NOTE 重要: =benchmark-init= 必須在您的設定檔中盡可能
  ;; 早地載入，才能測量所有 =load= 和 =require= 調用。
  :config
  ;; 啟動後停用資料收集
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

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

(use-package eat
  ;; NOTE A better terminal may be vterm with indirect-edit enhancement,
  ;; because its line-mode is worse than EAT's line-mode.
  :defer t
  :config
  (add-hook 'eat-exec-hook (lambda (x)
                             (declare (ignore x))
                             (eat-line-mode))))

(use-package which-key
  :defer nil
  :config
  (setq which-key-min-display-lines 1)
  (which-key-mode))

(use-package helpful
  :defer t)

(use-package general
  :defer nil
  :config
  (general-create-definer my-leader
    :states '(normal visual emacs)
    :prefix "SPC")
  (my-leader
   ":" '(execute-extended-command :which-key "M-x") ; FIXME :which-key didn't seem to work here
   "x" '(my/toggle-scratch-buffer :which-key "toggle scratch buffer")
   "<" '(consult-buffer :which-key "switch buffer")

   ;; Buffer
   "b" '(:ignore t :which-key "Buffer")
   "bb" '(switch-to-buffer :which-key "Switch")
   "bk" '(kill-current-buffer :which-key "Kill")
   "bi" '(ibuffer :which-key "ibuffer")
   
   ;; Terminal
   "t" '(:ignore t :which-key "Terminal")
   "tt" '(eat :which-key "New Eat")
   "tT" '(eat-terminal-toggle :which-key "Toggle Eat")
   
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

;; 簡潔的完成介面
(use-package vertico
  :defer t
  :init (vertico-mode))

;; Orderless - 強大的模糊匹配
(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  ;; (orderless-mode) ; FIXME not present
  ) 

;; Marginalia - 為 Vertico 候選列表添加額外資訊
(use-package marginalia
  :defer t
  :after vertico
  :init
  (marginalia-mode))

;; Consult - 多功能互動式命令
(use-package consult
  :defer t
  :after (vertico orderless) ;; 確保在 vertico 和 orderless 之後加載
  :config
  ;; 可以在這裡定義 consult 相關的鍵綁定，如果 general.el 中未定義
  ;; 確保 consult 的命令被 general.el 拾取或手動綁定
  ;; 例如：(global-set-key (kbd "C-c C-r") 'consult-recent-files)
  )

(use-package gruvbox-theme
  :defer nil
  :config
  (load-theme 'gruvbox-dark-hard t)) ; Or 'gruvbox-dark-medium', 'gruvbox-light-hard', etc.

;; --- 警告：不要在此行以下增加 LSP 或任何程式語言插件 ---
;; TODO Make warnings or errors explicit for extreme stability.
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1024 1024))))
