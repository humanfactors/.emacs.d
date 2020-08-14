(defun my/select-current-line-and-forward-line ()
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position))
  (activate-mark)
  (forward-line)
  (append-to-buffer "packs_final.R" (region-beginning) (region-end))
  (set-mark-command nil)
  (deactivate-mark))
;; Note that I would not recommend binding this command to `C-l'.
;; From my personal experience, the default binding to `C-l' to
;; `recenter-top-bottom' is very useful.
(global-set-key (kbd "C-l") #'my/select-current-line-and-forward-line)
