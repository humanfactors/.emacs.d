

;;; mdw-vibes.el
;;
;;; Code:

(load-theme 'spacemacs-dark)


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

(provide 'mdw-vibes)


;;; mdw-vibes.el ends here
