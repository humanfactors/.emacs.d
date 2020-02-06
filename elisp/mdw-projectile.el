;; Config for Projectile
;; (projectile-mode +1)
(counsel-projectile-mode +1)

;; Main keybind
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("~/MissionCode/FIPS" "~/Code" "~/Code/R"))
(setq projectile-sort-order 'recentf)
(setq projectile-indexing-method 'hybrid)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-switch-project-action #'projectile-find-dir)
(setq projectile-find-dir-includes-top-level t)


(setq counsel-projectile-find-dir-includes-top-level t)

(defun ivy-read-action-format-columns (actions)
  "Create a docstring from ACTIONS, using several columns if needed to preserve `ivy-height'.

ACTIONS is a list.  Each list item is a list of 3 items: key (a
string), cmd and doc (a string)."
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
			   
(setq ivy-read-action-format-function 'ivy-read-action-format-columns)

(provide 'mdw-projectile)
