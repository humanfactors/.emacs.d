;; General mode configuation

;; smart-parens

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-default-state 'emacs)
  (setq evil-toggle-key "<f9>")

  :config ;; tweak evil after loading it
  ;; Clear any insert bindings just in case
  (setcdr evil-insert-state-map nil)
  (setq evil-insert-state-map (make-sparse-keymap))
  (setq evil-emacs-state-cursor '(bar)) ;; I want my bar back
  
  ;; (setq evil-insert-state-map (make-sparse-keymap))
  ;; (define-key evil-emacs-state-map (kbd "<f12>") 'evil-normal-state)

  (global-set-key (kbd "<f12>") 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
  (define-key evil-normal-state-map "\C-n" 'evil-next-line)
  (defalias 'evil-insert-state 'evil-emacs-state)
  (evil-mode 1))



(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "M-C-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-C-[") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-C-{") 'sp-wrap-curly)
  :diminish smartparens-mode)
  ;; (("C-M-f" . sp-forward-sexp)
  ;;  ("C-M-b" . sp-backward-sexp)
  ;;  ("C-M-n" . sp-up-sexp)
  ;;  ("C-M-d" . sp-down-sexp)
  ;;  ("C-M-u" . sp-backward-up-sexp)
  ;;  ("C-M-p" . sp-backward-down-sexp)
  ;;  ("C-M-w" . sp-copy-sexp)
  ;;  ("M-s" . sp-splice-sexp)
  ;;  ("M-r" . sp-splice-sexp-killing-around)
  ;;  ("C-M-t" . sp-transpose-sexp))

(use-package writeroom-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))
  (setq writeroom-mode-line t)
  (setq writeroom-mode-line t)
  :bind (
	 ("<f7>-w" . writeroom-mode)
	 )
  )

;; ag.el
(use-package ag
  :ensure t
  :init
  (setq ag-executable "ag.exe"))

(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
        ("C-S-Z" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

(require 'w32-browser)

(use-package general)

;; Ace Window
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))


(use-package neotree
  :bind ("<f8>" . neotree-toggle))


(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package neotree
  :ensure t
  :defer t  )

(use-package emojify
  :ensure t
  :config
  (setq emojify-emoji-styles "unicode")
  (setq emojify-display-style "unicode")
  (setq  emojify-prog-contexts "comments")
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key ivy-minibuffer-map (kbd "M-y") 'ivy-next-line)
  (ivy-mode))

(use-package counsel
  :after ivy
  :init
  (with-eval-after-load 'counsel
    (setq ivy-initial-inputs-alist nil))
  :config
  (define-key counsel-find-file-map  (kbd "<backspace>") 'counsel-up-directory)
  (define-key counsel-find-file-map  (kbd "<backtab>") 'counsel-up-directory)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (counsel-mode))


;; (global-set-key  "\M-y" 'yank-pop) ; Unrequired fix for crappy popmenu

(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-allow-multiple-replacements t)
  (which-key-mode 1))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
    (setq company-idle-delay              0.3
          company-minimum-prefix-length   3
          company-show-numbers            t
          company-dabbrev-downcase        nil))


(use-package projectile
	:preface
	(defun ivy-read-action-format-columns (actions)
	  "Create a docstring from ACTIONS, using several columns if needed to preserve `ivy-height'.

	ACTIONS is a list.  Each list item is a list of 3 items: key (a string), cmd and doc (a string)."
	(let ((length (length actions))
		(i 0)
		(max-rows (- ivy-height 1))
		rows cols col lwidth rwidth)
		(while (< i length)
		(setq col (cl-subseq actions i (min length (cl-incf i max-rows))))
		(setq lwidth (apply 'max (mapcar (lambda (x)
						(length (nth 0 x)))
						col)))
		(setq rwidth (apply 'max (mapcar (lambda (x)
						(length (nth 2 x)))
						col)))
		(setq col (mapcar (lambda (x)
				(format (format "%%%ds: %%-%ds" lwidth rwidth)
					(propertize (car x) 'face 'ivy-action)
					(nth 2 x)))
				col))
		(cond
		((null rows)
		(setq rows (length col)))
		((< (length col) rows)
		(setq col (append col (make-list (- rows (length col)) "")))))
		(push col cols))
		(format "%s\n%s\n"
				(if (eq this-command 'ivy-read-action)
			"Select action: "
				(ivy-state-current ivy-last))
			(mapconcat 'identity
				(apply 'cl-mapcar
					(lambda (&rest args)
					(mapconcat 'identity args " | "))
					(nreverse cols))
				"\n"))))
	:config
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(setq projectile-project-search-path '("~/Code"))
	(setq projectile-sort-order 'recentf)
	(setq projectile-indexing-method 'hybrid)
	(setq projectile-enable-caching t)
	(setq projectile-completion-system 'ivy)
	(setq projectile-switch-project-action #'projectile-find-dir)
	(setq projectile-find-dir-includes-top-level t)
	(setq counsel-projectile-find-dir-includes-top-level t)
	(counsel-projectile-mode +1)
	(setq ivy-read-action-format-function 'ivy-read-action-format-columns)
)


(use-package ess-r-mode
  :ensure ess
  :defer t
  :preface
  (defun then_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (defun tide-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert "<- "))
  ;; Mark a word at a point ==============================================
  ;; http://www.emacswiki.org/emacs/ess-edit.el
  (defun ess-edit-word-at-point ()
    (save-excursion
      (buffer-substring
      (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
      (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

  (defun ess-eval-word ()
    (interactive)
    (let ((x (ess-edit-word-at-point)))
      (ess-eval-linewise (concat x))))
  :init
  (setq ess-tab-complete-in-script t)
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-disable-underscore-assign t)
  (setq ess-smart-S-assign-key nil)
  (setq ess-eval-visibly 'nowait)
  (setq ess-indent-level 2)
  (setq tab-width 2)
  (setq ess-fancy-comments nil)
  :config
  (define-key ess-r-mode-map  (kbd "C-c r") 'ess-eval-word)
  (define-key ess-r-mode-map  (kbd "C-S-M") 'then_R_operator)
  (define-key ess-r-mode-map  (kbd "C-'") 'tide-insert-assign))


;; Markdown
(use-package markdown-mode
  :defer t
  :mode ("\\`README\\.md\\'" . gfm-mode)
  :ensure t
  :init
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-list-indent-width 4)
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
  :config
  ;; Fix inline codeblocks being split in markdown mode in Rmarkdown documents when filling
  (add-hook 'fill-nobreak-predicate
            #'markdown-inline-code-at-point-p))

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (global-visual-line-mode 1))


(use-package deft
  :ensure t
  :defer t
  :config
  (setq deft-extensions '("org" "md" "tex" "txt"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-use-filename-as-title t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-text-mode 'org-mode)
  (define-key deft-mode-map (kbd "C-c C-m") 'deft-new-file-named))


(use-package biblio
  :ensure t
  :defer t)

;; (setq org-ref-default-bibliography '("~/Papers/references.bib")
;;       org-ref-pdf-directory "~/Papers/"
;;       org-ref-bibliography-notes "~/Papers/notes.org")

(use-package org-ref
  :ensure t
  :after (biblio)
  :init
  (add-hook 'bibtex-mode-hook '(require 'org-ref-bibtex)))

(use-package avy
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key "C-;" 'avy-goto-char))

(provide 'mdw-modes)
