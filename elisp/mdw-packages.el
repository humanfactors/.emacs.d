
;;; mdw-packages.el
;;
;;; Code:


;; Setup package manager
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))  
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(setq my-packages 
      '(
	;; rainbow-mode         ;; Highlight colors in code
	ace-window
	ag
	company              ;; Auto completion
	counsel
	counsel-projectile
	deft
	dracula-theme
	ess
	expand-region        ;; Select larger and larger region
	free-keys            ;; Show what key combinations that are not taken
	general
	ivy
	markdown-mode
	org
	projectile
	rainbow-delimiters   ;; Colorful parens
	smartparens          ;; Slurp and barf
	smex                 ;; Better help in minibuffer
	spaceline
	;; spacemacs-theme
	swiper                ;; Better search in buffer
	undo-tree            ;; Show a graph of edits that can be undone
	visual-fill-column
	which-key
	)
      )

					; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

					; Install all missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'mdw-packages)
;;; mdw-packages.el ends here

