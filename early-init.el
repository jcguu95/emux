;;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
;; (setq warning-minimum-level :debug)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 提高 GC 門檻以加速啟動
;; 啟動完成後調回合理的數值（這段通常留在 init.el 的最後面）
(setq gc-cons-threshold (* 100 1024 1024))

(setq inhibit-startup-screen t)         ; 禁用啟動畫面
(setq initial-scratch-message nil)      ; 讓 scratch 乾淨一點
(setq frame-inhibit-implied-resize t)   ; 禁止縮放視窗時的延遲

(when (display-graphic-p)
  (set-frame-parameter nil 'alpha-background 85))

(provide 'early-init)
