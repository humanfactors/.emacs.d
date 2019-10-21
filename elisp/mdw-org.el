;; MDW Org Configuration

(require 'org)

(setq org-directory "~/Dropbox/org/"
      org-support-shift-select 1)

(setq org-agenda-files (list "~/Dropbox/org/"
			     "~/Dropbox/org/Committees/"))


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


(provide 'mdw-org)
