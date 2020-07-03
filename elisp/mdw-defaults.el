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

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq save-interprogram-paste-before-kill t)
;; buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Startup speedups and fun
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

;; y or n is fine
(defalias 'yes-or-no-p 'y-or-n-p)

; BLINK BLINK BLINK NO NO NO
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;; Fringes
(fringe-mode '(25 . 0))

;; Little modes and fixes
(auto-save-mode 0)
(delete-selection-mode 1)
(tool-bar-mode -1)
(when (not (display-graphic-p))
  (menu-bar-mode -1))
(scroll-bar-mode 0)
(set-fringe-mode 1)
(hl-line-mode 1)

(global-auto-revert-mode 1) ;; change buffer automatically when file changes on disk
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.

(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-follow-symlinks t)

(setq backup-directory-alist '(("." . "~/.saves/")))    ; don't litter my fs tree
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; Disable lockfiles
(setq create-lockfiles nil)
;; (setq make-backup-files nil)

;; Editing 
(setq fill-column 120)
(setq visual-fill-column-width 120)
(auto-fill-mode 0)
(electric-pair-mode 1)
(setq word-wrap 1)

;; Copy Pasting
(setq select-enable-primary t)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; Dired doens't need to be filled unless it's insanity
(add-hook 'dired-mode-hook
          (lambda () 
             (setq fill-column 140)))


(setq dired-dnd-protocol-alist nil) ; Dired disable drag n drop copy

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
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))



;; Mouse wheel
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Clock
(setq display-time-24hr-format t)

;; initial window
(setq initial-frame-alist
      '(
        (width . 140) ; character
        (height . 40) ; lines
        ))

;; Initial buffer
;; (setf initial-buffer-choice (lambda () (dired "~/Dropbox/org")))
(defun display-startup-echo-area-message ()
  (message "Welcome to the Matrix."))

(provide 'mdw-defaults)
;;; mdw-defaults.el ends here

