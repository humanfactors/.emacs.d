;;; mdw-packages.el
;;
;;; Code:


;; Setup package manager
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))


;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(setq my-packages
    '(
        ;; rainbow-mode         ;; Highlight colors in code
        ;; ace-window
        ;; ag
        ;; company              ;; Auto completion
        ;; counsel
        ;; counsel-projectile
        ;; deft
        ;; dracula-theme
        ;; doom-themes
        ;; doom-modeline
        beacon
        ess
        expand-region        ;; Select larger and larger region
        ;; free-keys            ;; Show what key combinations that are not taken
        ;; general
        ;; ivy
        ;; markdown-mode
        ;; org
        ;; neotree
        ;; projectile
        ;; rainbow-delimiters   ;; Colorful parens
        smartparens          ;; Slurp and barf
        ;; smex                 ;; Better help in minibuffer
        use-package
        ;; spacemacs-theme
        ;; swiper                ;; Better search in buffer
        undo-tree            ;; Show a graph of edits that can be undone
        visual-fill-column
        which-key
    ))




;; Install all missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
  (package-refresh-contents)
    (package-install package)))

; Ensure use-package is required
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'mdw-packages)
;;; mdw-packages.el ends here
