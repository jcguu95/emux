;;; -*- lexical-binding: t; -*-
;;; Usage: emacs --init-dir=/path/to/this/dir/

(setq warning-minimum-level :debug)
(setq debug-on-error t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-straight)
(require 'init-benchmark)               ; NOTE 平常沒必要時，把這個刪掉以增加 booting 效率。
(require 'init-ui)
(require 'init-evil)
(require 'init-eat)                     ; EAT, an emacs terminal emulator with sane LINE-MODE.
(require 'init-bindings)
(require 'init-window)
(require 'init-completion)

;; --- System Clipboard ---
;; TODO Integrate `emacs -nw` and system clipboard (#'evil-yank and
;; #'evil-paste-after). See how doomemacs did this.

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1024 1024))))
