

;;; mdw-vibes.el
;;
;;; Code:

;; Setting themes
(require 'cl)
(load-theme 'dracula)
;; (load-theme 'leuven)

;; (setq cycle-themes-theme-list
      ;; '(leuven spacemacs-dark spacemacs-light))

;; (require 'cycle-themes)
;; (cycle-themes-mode)

;; Initial buffer

;; (setf initial-buffer-choice (lambda () (dired "~/Dropbox/org")))
(defun display-startup-echo-area-message ()
  (message "Welcome to the Matrix."))
  
;; initial window
(setq initial-frame-alist
      '(
        (width . 140) ; character
        (height . 40) ; lines
        ))

;; Set default font
(set-face-attribute 'default nil
                    :family "Roboto Mono for Powerline"
                    :height 120
                    :weight 'normal
                    :width 'normal)


;; Rainbow parens
(require 'rainbow-delimiters)
(defun activate-rainbow-hooks ()
  (interactive)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))
(activate-rainbow-hooks)


;; Mouse wheel
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse


;; Clock
(setq display-time-24hr-format t)

;; Modeline
(require 'spaceline-config)
(spaceline-emacs-theme)

(provide 'mdw-vibes)


;;; mdw-vibes.el ends here
