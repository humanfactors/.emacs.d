;;; mdw-defaults.el --- My set of sane defaults
;;
;;; Code:

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Editing config
(setq mouse-yank-at-point nil) ;; Fixes bug associated with middle click paste for spell check
(setq tab-width 4) ; or any other preferred value


;; Startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; y or n is fine
(defalias 'yes-or-no-p 'y-or-n-p)

;; Little modes and fixes
(auto-save-mode 0)
(delete-selection-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(set-fringe-mode 1)
(hl-line-mode 1)

(global-auto-revert-mode 1) ;; change buffer automatically when file changes on disk
(global-visual-line-mode 1)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-follow-symlinks t)

;; Disable lockfiles
(setq create-lockfiles nil)
;; (setq make-backup-files nil)

;; Editing 
(setq fill-column 120)
(auto-fill-mode 0)
(electric-pair-mode 1)


;; Quality of Life
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq undo-limit 999999)
(setq compilation-ask-about-save nil)
(setq echo-keystrokes 0.1)
(setq pcomplete-ignore-case t) ;; ignore case when auto completing in shell
(setq ns-pop-up-frames nil) ;; open files in same frame (don't create new separate ones)


;; Allow 20MB of memory (instead of 0.76MB default) before calling
;; garbage collection. This means GC runs less often, which speeds
;; up some operations
(setq gc-cons-threshold 20000000)
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))


(provide 'mdw-defaults)
;;; mdw-defaults.el ends here

