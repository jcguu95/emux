;;; -*- lexical-binding: t; -*-

;;; This project is stale. I am not sure if I should reinvent a mini-doom
;;; after de-dooming.

(push '(menu-bar-lines . 0) default-frame-alist) 
(push '(tool-bar-lines . 0) default-frame-alist)  
(push '(vertical-scroll-bars) default-frame-alist) 
(set-fringe-mode 0)

;;; TODO Learn from https://github.com/doomemacs/doomemacs/tree/master/modules
;; 
;;; Urgent
;; 
;; evil
;; org
;; ibuffer
;; magit
;; completion/vertico
;; spell
;; llm (gptel)
;; 
;;; crucial
;; 
;; hl-todo
;; elisp
;; common-lisp
;; vterm
;; workspace
;; window-select
;; 
;;; nonurgent
;; 
;; macos
;; tty
;; eval
;; dired / dirvish
;; unicode
;; modeline
;; pass
;; tmux
;; pdf

(provide 'init-extended-profile)
