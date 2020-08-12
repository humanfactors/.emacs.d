;; MDW Org Configuration

(require 'org)

(use-package org
  :defer t
  :ensure org-plus-contrib
  :config
  (setf org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (setq org-hierarchical-todo-statistics t)
  (setq org-bullets-mode nil)
  (setq org-indent-mode nil)
  (setq org-hide-leading-stars nil)
  (setq org-support-shift-select t)
  (setq org-cycle-separator-lines 1)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-indented t)

  (custom-set-faces
   '(org-done ((t (:weight normal :strike-through t)))))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-directory "~/Dropbox/org/"
	org-support-shift-select 1)

  (setq org-agenda-files (list "~/Dropbox/org/"
			       "~/Dropbox/org/Committees/"))

  (add-to-list 'org-structure-template-alist
	       (list "ti" "#+TITLE: ?\n"))


  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

  :init

  (defun org-wrap-quote ()
    (interactive)
    (let ((start (min (point) (mark)))
	  (end (max (point) (mark))))
      (goto-char end)
      (unless (bolp)
	(newline))
      (insert "#+END_QUOTE\n")
      (goto-char start)
      (unless (bolp)
	(newline))
      (insert "#+BEGIN_QUOTE\n")))


  (defun org-wrap-source ()
    (interactive)
    (let ((start (min (point) (mark)))
	  (end (max (point) (mark))))
      (goto-char end)
      (unless (bolp)
	(newline))
      (insert "#+END_SRC\n")
      (goto-char start)
      (unless (bolp)
	(newline))
      (insert "#+BEGIN_SRC\n")))

  (defmacro mdw|org-emphasize (fname char)
    "Make function for setting the emphasis in org mode"
    `(defun ,fname () (interactive)
	    (org-emphasize ,char)))

  (mdw|org-emphasize mdw/org-underline ?_)
  (mdw|org-emphasize mdw/org-strike-through ?+)


  (add-hook 'org-mode-hook 'turn-on-flyspell)

  (general-define-key
   :keymaps 'org-mode-map
   "C-c b" (mdw|org-emphasize mdw/org-bold ?*)
   "C-c `" (mdw|org-emphasize mdw/org-code ?~)
   "C-c i" (mdw|org-emphasize mdw/org-italic ?/)
   "C-c l" (mdw|org-emphasize mdw/org-literal ?=)))

  ;; '(org-headline-done
  ;;            ((((class color) (min-colors 16) (background dark))
  ;;               (:foreground "LightSalmon" :strike-through t)))))






  (provide 'mdw-org)
