;; MDW Org Configuration

(require 'org)

(custom-set-faces
 '(org-done ((t (:weight normal
                 :strike-through t)))))
 ;; '(org-headline-done 
 ;;            ((((class color) (min-colors 16) (background dark)) 
 ;;               (:foreground "LightSalmon" :strike-through t)))))

(setq org-directory "~/Dropbox/org/"
      org-support-shift-select 1)

(setq org-agenda-files (list "~/Dropbox/org/"
			     "~/Dropbox/org/Committees/"))

(setf org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
(setq org-hierarchical-todo-statistics t)
(setq org-bullets-mode nil)
(setq org-indent-mode t)
(setq org-hide-leading-stars 0)
(setq org-support-shift-select t)
(setq org-cycle-separator-linesq 1)

(add-to-list 'org-structure-template-alist
	     (list "ti" "#+TITLE: ?\n"))


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

(general-define-key
 :keymaps 'org-mode-map
 "C-c b" (mdw|org-emphasize mdw/org-bold ?*)
 "C-c `" (mdw|org-emphasize mdw/org-code ?~)
 "C-c i" (mdw|org-emphasize mdw/org-italic ?/)   
 "C-c l" (mdw|org-emphasize mdw/org-literal ?=))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

(provide 'mdw-org)
