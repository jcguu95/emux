;;; -*- lexical-binding: t; -*-
;;; Usage: emacs --init-dir=/path/to/this/dir/

;;; 
(require 'init-straight)                ; NOTE Emacs' Package Manager
(require 'init-benchmark)               ; NOTE 平常沒必要時，把這個刪掉以增加 booting 效率。

;;; Core Profile
;; 
;;  A high-performance, tmux-inspired Emacs environment optimized for
;;  terminal-based workflows. It provides a lightweight "Multiplexer"
;;  experience that prioritizes rapid startup, modal efficiency, and deep
;;  system introspection. It takes ~170ms to boot on a Macbook M2 (TODO It
;;  feels longer though).
;; 
(require 'init-core-profile)

;;; Extended Profile 
;; 
;; (require 'init-extended-profile)

;;; 
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1024 1024))))
