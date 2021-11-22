;; General mode configuation



(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-respect-visual-line-mode t)
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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; smart-parens
;; (use-package smartparens
;;   :diminish smartparens-mode
;;   ;; :config (require 'smartparens-config)
;;   :config
;;   (smartparens-global-mode 1)
;;   (show-paren-mode t)
;;   (require 'smartparens-config)
;;   :init
;;   (define-key smartparens-mode-map (kbd "M-C-(") 'sp-wrap-round)
;;   (define-key smartparens-mode-map (kbd "M-C-[") 'sp-wrap-square)
;;   (define-key smartparens-mode-map (kbd "M-C-{") 'sp-wrap-curly))
;;   ;; :bind
  ;; (:map
  ;;  smartparens-mode-map
  ;;  ;; ("M-<delete>" . sp-unwrap-sexp)
  ;;  ("s-{" . sp-rewrap-sexp)))

(use-package smartparens
  :defer t
  :init
  (define-key smartparens-mode-map (kbd "M-C-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-C-[") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-C-{") 'sp-wrap-curly)
  (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  (bind-key "M-i" #'sp-change-inner)
  ;; (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  ;; (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  (bind-key "C-S-s" #'sp-splice-sexp)
  (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  :config
  (smartparens-global-mode t)
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)



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
  :bind (("<f7> w" . writeroom-mode)))

;; ag.el
(use-package ag
  :ensure t
  :init
  (when-system windows-nt
    (setq ag-executable "ag.exe")))

(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
    ("C-S-Z" . undo-tree-redo))
  :config
  (global-undo-tree-mode 1))

(when-system windows-nt
  (progn(require 'w32-browser)))

(use-package general
  :ensure t)

;; Ace Window
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))


;; (use-package neotree
;;   :bind ("<f8>" . neotree-toggle))


(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
     ("C-f" . swiper)
     ("C-r" . swiper)))

;; (use-package emojify
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq emojify-emoji-styles "unicode")
;;   (setq emojify-display-style "unicode")
;;   (setq  emojify-prog-contexts "comments")
;;   (add-hook 'after-init-hook #'global-emojify-mode))

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
  (setq enable-recursive-minibuffers t)
  (ivy-mode))

(use-package counsel
  :after ivy
  :init
  (with-eval-after-load 'counsel
    (setq ivy-initial-inputs-alist nil))
  :config
  (define-key counsel-find-file-map  (kbd "C-<backspace>") 'counsel-up-directory)
  ;; (define-key counsel-find-file-map  (kbd "<backtab>") 'counsel-up-directory)
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
  :defer t
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
    (setq company-idle-delay              0.5
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
    (setq projectile-find-dir-includes-top-level t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action #'projectile-find-dir)
    (setq projectile-find-dir-includes-top-level t)
    (counsel-projectile-mode +1)
    (setq counsel-projectile-find-dir-includes-top-level t)
    (setq ivy-read-action-format-function 'ivy-read-action-format-columns))

;;  :preface
;;  (defun ivy-read-action-format-columns (actions)
;;    "Create a docstring from ACTIONS, using several columns if needed to preserve `ivy-height'.

;;  ACTIONS is a list.  Each list item is a list of 3 items: key (a string), cmd and doc (a string)."
;;  (let ((length (length actions))
;;    (i 0)
;;    (max-rows (- ivy-height 1))
;;    rows cols col lwidth rwidth)
;;    (while (< i length)
;;    (setq col (cl-subseq actions i (min length (cl-incf i max-rows))))
;;  (setq lwidth (apply 'max (mapcar (lambda (x)
;;            (length (nth 0 x)))
;;            col)))
;;    (setq rwidth (apply 'max (mapcar (lambda (x)
;;            (length (nth 2 x)))
;;            col)))
;;    (setq col (mapcar (lambda (x)
;;        (format (format "%%%ds: %%-%ds" lwidth rwidth)
;;          (propertize (car x) 'face 'ivy-action)
;;          (nth 2 x)))
;;        col))
;;    (cond
;;    ((null rows)
;;    (setq rows (length col)))
;;    ((< (length col) rows)
;;    (setq col (append col (make-list (- rows (length col)) "")))))
;;    (push col cols))
;;    (format "%s\n%s\n"
;;        (if (eq this-command 'ivy-read-action)
;;      "Select action: "
;;        (ivy-state-current ivy-last))
;;      (mapconcat 'identity
;;        (apply 'cl-mapcar
;;          (lambda (&rest args)
;;          (mapconcat 'identity args " | "))
;;          (nreverse cols))
;;        "\n"))))
;;  :config
;;  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;  (setq projectile-project-search-path '("~/Code"))
;;  (setq projectile-sort-order 'recentf)
;;  (setq projectile-indexing-method 'hybrid)
;;  (setq projectile-enable-caching t)
;;  (setq projectile-completion-system 'ivy)
;;  (setq projectile-switch-project-action #'projectile-find-dir)
;;  (setq projectile-find-dir-includes-top-level t)
;;  (setq counsel-projectile-find-dir-includes-top-level t)
;;  (counsel-projectile-mode +1)
;;  (setq ivy-read-action-format-function 'ivy-read-action-format-columns)
;; )

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
  :custom-face
  (markdown-code-face ((t nil)))
  :config
  ;; Fix inline codeblocks being split in markdown mode in Rmarkdown documents when filling
  (add-hook 'fill-nobreak-predicate
        #'markdown-inline-code-at-point-p))

(use-package visual-fill-column
  :ensure t
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (global-visual-line-mode 1))


(use-package deft
  :ensure t
  :commands deft
  :defer t
  :init
  (setq deft-extensions '("org" "md" "tex" "txt"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-use-filename-as-title t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-text-mode 'org-mode)
  :bind (:map deft-mode-map
          ("C-c C-m" . deft-new-file-named)
          ("<backspace>" . deft-filter-decrement)))

(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )


(use-package clojure-mode
  :defer t
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

;; (use-package cider
;;   :ensure t)


(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package company-auctex
  :defer t
  :ensure t
  :init (company-auctex-init))

(use-package company-bibtex
  :commands company-bibtex
  :config (add-to-list 'company-backends #'company-bibtex))

(use-package auctex-latexmk
  :defer t
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(use-package tex
  :defer t
  :ensure auctex
  ;; :commands TeX-latex-mode
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


(use-package treemacs
  :ensure t
  :defer 10
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


    (when-system windows-nt (setq treemacs-python-executable (treemacs--find-python3)))

    ;; "python.exe"

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
  (:map treemacs-mode-map
    ("D" . treemacs-delete)
    ("d" . nil))
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
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-mode)
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

(use-package biblio
  :ensure t
  :defer t)

;; (setq org-ref-default-bibliography '("~/Papers/references.bib")
;;       org-ref-pdf-directory "~/Papers/"
;;       org-ref-bibliography-notes "~/Papers/notes.org")

;; (use-package org-ref
;;   :ensure t
;;   :after (biblio)
;;   :commands (org-ref-bibtex-next-entry
;;          org-ref-bibtex-previous-entry
;;          org-ref-open-in-browser
;;          org-ref-open-bibtex-notes
;;          org-ref-open-bibtex-pdf
;;          org-ref-bibtex-hydra/body
;;          org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
;;          org-ref-sort-bibtex-entry
;;          arxiv-add-bibtex-entry
;;          arxiv-get-pdf-add-bibtex-entry
;;          doi-utils-add-bibtex-entry-from-doi
;;          isbn-to-bibtex
;;          pubmed-insert-bibtex-from-pmid)
;;   :config
;;   (add-hook 'bibtex-mode-hook '(require 'org-ref-bibtex)))

(use-package avy
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key "C-;" 'avy-goto-char))



;; (global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
;; (setq hippie-expand-try-functions-list
;;       '(
;;  ;; Try to expand yasnippet
;;  yas-hippie-try-expand
;;  ;; Try to expand word "dynamically", searching the current buffer.
;;  try-expand-dabbrev
;;  ;; Try to expand word "dynamically", searching all other buffers.
;;  try-expand-dabbrev-all-buffers
;;  ;; Try to expand word "dynamically", searching the kill ring.
;;  try-expand-dabbrev-from-kill
;;  ;; Try to complete text as a file name, as many characters as unique.
;;  try-complete-file-name-partially
;;  ;; Try to complete text as a file name.
;;  try-complete-file-name
;;  ;; Try to expand word before point according to all abbrev tables.
;;  try-expand-all-abbrevs
;;  ;; Try to complete the current line to an entire line in the buffer.
;;  try-expand-list
;;  ;; Try to complete the current line to an entire line in the buffer.
;;  try-expand-line
;;  ;; Try to complete as an Emacs Lisp symbol, as many characters as
;;  ;; unique.
;;  try-complete-lisp-symbol-partially
;;  ;; Try to complete word as an Emacs Lisp symbol.
;;  try-complete-lisp-symbol))

;; (general-define-key "C-;" 'company-complete)


(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("M-]" . er/mark-outside-pairs)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (use-package ivy-rich
  ;; :init (ivy-rich-mode 1))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Hello Michael, Welcome to Emacs.")
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (defun switch-to-dashboard ()
    (interactive)
    (require 'dashboard)
    (switch-to-buffer "*dashboard*")
    (dashboard-insert-startupify-lists))
  (dashboard-setup-startup-hook))

;; (use-package unicode-fonts
   ;; :ensure t
   ;; :config
    ;; (unicode-fonts-setup))

;; (use-package xahk-mode)

(provide 'mdw-modes)
