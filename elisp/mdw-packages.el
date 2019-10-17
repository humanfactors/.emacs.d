
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
        ivy
        deft
        general
        ace-window
        which-key
        counsel
        swiper                ;; Better search in buffer
        org
        smartparens          ;; Slurp and barf
        rainbow-delimiters   ;; Colorful parens
        ;; rainbow-mode         ;; Highlight colors in code
        ;; undo-tree            ;; Show a graph of edits that can be undone
        smex                 ;; Better help in minibuffer
        company              ;; Auto completion
        expand-region        ;; Select larger and larger region
        free-keys            ;; Show what key combinations that are not taken
        ;; ace-window           ;; Select window with number keys
        markdown-mode
        ess
        spacemacs-theme
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

