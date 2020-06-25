;; General mode configuation

;; smart-parens

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "M-C-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-C-[") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-C-{") 'sp-wrap-curly)
  :diminish smartparens-mode)

  ;; ( ;("C-M-f" . sp-forward-sexp)
  ;;  ;("C-M-b" . sp-backward-sexp)
  ;;  ("C-M-n" . sp-up-sexp)
  ;;  ("C-M-d" . sp-down-sexp)
  ;;  ("C-M-u" . sp-backward-up-sexp)
  ;;  ("C-M-p" . sp-backward-down-sexp)
  ;;  ("C-M-w" . sp-copy-sexp)
  ;;  ("M-s" . sp-splice-sexp)
  ;;  ("M-r" . sp-splice-sexp-killing-around)
  ;;  ("C-M-t" . sp-transpose-sexp))

;; ag.el
(use-package ag
  :ensure t
  :init
  (when-system windows-nt
    (setq ag-executable "ag.exe"))
  )

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


;; (use-package neotree
;;   :bind ("<f8>" . neotree-toggle))


(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
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
	(setq projectile-project-search-path '("~/Code" "~/Code/R"))
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


(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  ;; :init
  ;; (add-hook 'clojure-mode-hook #'yas-minor-mode)
  ;; (add-hook 'clojure-mode-hook #'linum-mode)
  ;; (add-hook 'clojure-mode-hook #'subword-mode)
  ;; (add-hook 'clojure-mode-hook #'smartparens-mode)
  ;; (add-hook 'clojure-mode-hook #'eldoc-mode)

  )

(use-package cider
  :ensure t)


(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq reftex-plug-into-AUCTeX t)
	    (setq TeX-view-program-selection '((output-pdf "Evince")))
	    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
	    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
	    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
	    (add-hook 'LaTeX-mode-hook 'yas-minor-mode-on)
	    (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
	    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))))

(use-package ivy-bibtex
  :defer t
  :ensure t
  :bind ( :map  LaTeX-mode-map
		("C-\ l b" . ivy-bibtex)))

(use-package smartparens
  :init
  (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  (bind-key "C-S-s" #'sp-splice-sexp)
  (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  :config
  (smartparens-global-mode t)
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode
  )


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("<f8>" . 'treemacs)
        ("C-\ t" . 'treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


;; (use-package paredit
  ;; :ensure t
  ;; :config
  ;; (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  ;; (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  ;; (add-hook 'ielm-mode-hook #'paredit-mode)
  ;; (add-hook 'lisp-mode-hook #'paredit-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))


(provide 'mdw-modes)
