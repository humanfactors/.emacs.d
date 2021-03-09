;;; mdw-vibes.el
;; Things that make this look sexy
;;; Code:

;; Setting themes
;; (require 'cl)

;; Set default font
(add-to-list 'default-frame-alist '(font . "Iosevka Slab"))
;; (add-to-list 'default-frame-alist '(font . "RobotoMono NF"))

(display-battery-mode 1)

(set-face-attribute 'default nil
            :family "Iosevka Slab"
            :height 135
            :weight 'normal
            :width 'normal)

(fringe-mode 8)
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'whitespace-cleanup)))

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-project-detection 'project)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
    doom-themes-enable-italic t)) ; if nil, italics is universally disabled


(use-package beacon
  :init
  (beacon-mode 1)
  (setq beacon-color "#50fa7b"
    beacon-lighter ""
    beacon-blink-duration 0.2
    beacon-size 18))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (setq doom-dracula-padded-modeline t
    doom-dracula-colorful-headers t)
  (doom-themes-org-config))

(use-package rainbow-mode
  :defer t
  :ensure t
  :hook
  (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode) . display-line-numbers-mode))




(provide 'mdw-vibes)


;;; mdw-vibes.el ends here
