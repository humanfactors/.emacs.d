(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
	  (skip-chars-backward "\n")
	  (setq $p3 (point))
	  (skip-chars-forward "\n")
	  (setq $p4 (point))
	  (delete-region $p3 $p4)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.
Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-20"
  (interactive)
  (let* (
	 ($eol-count 0)
	 ($p0 (point))
	 $p1 ; whitespace begin
	 $p2 ; whitespace end
	 ($charBefore (char-before))
	 ($charAfter (char-after ))
	 ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
	 $just-1-space-p
	 )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
	  (delete-horizontal-space)
	(progn (delete-horizontal-space)
	       (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
	  (delete-horizontal-space)
	(progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
	  (delete-horizontal-space)
	(progn
	  (xah-delete-blank-lines)
	  (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
	  (delete-horizontal-space)
	(progn
	  (goto-char $p2)
	  (search-backward "\n" )
	  (delete-region $p1 (point))
	  (insert "\n"))))
     (t (progn
	  (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($compact-p
	  (if (eq last-command this-command)
	      (get this-command 'compact-p)
	    (> (- (line-end-position) (line-beginning-position)) fill-column)))
	 (deactivate-mark nil)
	 ($blanks-regex "\n[ \t]*\n")
	 $p1 $p2
	 )
    (if (use-region-p)
	(progn (setq $p1 (region-beginning))
	       (setq $p2 (region-end)))
      (save-excursion
	(if (re-search-backward $blanks-regex nil "move")
	    (progn (re-search-forward $blanks-regex)
		   (setq $p1 (point)))
	  (setq $p1 (point)))
	(if (re-search-forward $blanks-regex nil "move")
	    (progn (re-search-backward $blanks-regex)
		   (setq $p2 (point)))
	  (setq $p2 (point)))))
    (if $compact-p
	(fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
	(fill-region $p1 $p2)))
    (put this-command 'compact-p (not $compact-p))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.
URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (@begin @end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.
URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region @begin @end)))

(defun xah-reformat-lines ( &optional @length)
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
When the command is called for the first time, it checks the current line's length to decide to go into 1 line or multiple lines. If current line is short, it'll reformat to 1 long lines. And vice versa.
Repeated call toggles between formatting to 1 long line and multiple lines.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2018-09-01"
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
	 (@length (if @length
		      @length
		    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column )))
	 (is-longline-p
	  (if (eq last-command this-command)
	      (get this-command 'is-longline-p)
	    (> (- (line-end-position) (line-beginning-position)) @length)))
	 ($blanks-regex "\n[ \t]*\n")
	 $p1 $p2
	 )
    (if (use-region-p)
	(progn (setq $p1 (region-beginning))
	       (setq $p2 (region-end)))
      (save-excursion
	(if (re-search-backward $blanks-regex nil "move")
	    (progn (re-search-forward $blanks-regex)
		   (setq $p1 (point)))
	  (setq $p1 (point)))
	(if (re-search-forward $blanks-regex nil "move")
	    (progn (re-search-backward $blanks-regex)
		   (setq $p2 (point)))
	  (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
	  (xah-reformat-to-multi-lines $p1 $p2 @length)
	(if is-longline-p
	    (xah-reformat-to-multi-lines $p1 $p2 @length)
	  (xah-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))

(defun xah-reformat-whitespaces-to-one-space (@begin @end)
  "Replace whitespaces by one space.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while
	  (search-forward "\n" nil "move")
	(replace-match " "))
      (goto-char (point-min))
      (while
	  (search-forward "\t" nil "move")
	(replace-match " "))
      (goto-char (point-min))
      (while
	  (re-search-forward "  +" nil "move")
	(replace-match " ")))))

(defun xah-reformat-to-multi-lines ( &optional @begin @end @min-length)
  "Replace spaces by a newline at places so lines are not long.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2018-09-01"
  (interactive)
  (let (
	$p1 $p2
	($blanks-regex "\n[ \t]*\n")
	($minlen (if @min-length
		     @min-length
		   (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column))))
    (if (and  @begin @end)
	(setq $p1 @begin $p2 @end)
      (if (region-active-p)
	  (progn (setq $p1 (region-beginning) $p2 (region-end)))
	(save-excursion
	  (if (re-search-backward $blanks-regex nil "move")
	      (progn (re-search-forward $blanks-regex)
		     (setq $p1 (point)))
	    (setq $p1 (point)))
	  (if (re-search-forward $blanks-regex nil "move")
	      (progn (re-search-backward $blanks-regex)
		     (setq $p2 (point)))
	    (setq $p2 (point))))))
    (save-excursion
      (save-restriction
	(narrow-to-region $p1 $p2)
	(goto-char (point-min))
	(while
	    (re-search-forward " +" nil "move")
	  (when (> (- (point) (line-beginning-position)) $minlen)
	    (replace-match "\n" )))))))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.
URL `http://ergoemacs.org/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
	(progn
	  (setq $p1 (region-beginning))
	  (setq $p2 (region-end)))
      (save-excursion
	(if (re-search-backward "\n[ \t]*\n" nil "move")
	    (progn (re-search-forward "\n[ \t]*\n")
		   (setq $p1 (point)))
	  (setq $p1 (point)))
	(re-search-forward "\n[ \t]*\n" nil "move")
	(skip-chars-backward " \t\n" )
	(setq $p2 (point))))
    (save-excursion
      (save-restriction
	(narrow-to-region $p1 $p2)
	(goto-char (point-min))
	(while (re-search-forward " +" nil t)
	  (replace-match "\n" ))))))
