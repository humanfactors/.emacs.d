;;; mdw-vibes.el
;; Things that make this look sexy
;;; Code:

;; Setting themes
(require 'cl)

;; Set default font
(add-to-list 'default-frame-alist '(font . "Fira Code 13"))
(set-face-attribute 'default nil
		    :family "Fira Code"
		    :height 130
		    :weight 'normal
		    :width 'normal)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; (use-package fira-code-mode
  ;; :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  ;; :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-project-detection 'project)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

(use-package beacon
  :init
  (beacon-mode 1)
  (setq beacon-color "#50fa7b"
	beacon-lighter ""
	beacon-blink-duration 0.3))

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
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

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
