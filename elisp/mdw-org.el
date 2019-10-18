;; MDW Org Configuration

(setq org-directory "~/Dropbox/org/")

(setq org-agenda-files (list "~/Dropbox/org/"
			     "~/Dropbox/org/Committees/"))

(setf org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
(setq org-hierarchical-todo-statistics t)
(setq org-bullets-mode nil)
(setq org-support-shift-select t)
(setq org-cycle-separator-lines 1)


(provide 'mdw-org)
