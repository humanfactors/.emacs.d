;;; mkw-utilities.el --- mkw-utilities description
;;
;;; Code:

;; Utility Functionality
(defun micah-timestamp ()
  "Insert a timestamp at the current point.
Note no attempt to go to beginning of line and no added carriage return.
Uses `bjk-timestamp-format' for formatting the date/time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M" (current-time))))

(defun micah-filetimestamp ()
  "Insert a timestamp at the current point.
Note no attempt to go to beginning of line and no added carriage return.
Uses `bjk-timestamp-format' for formatting the date/time."
  (interactive)
  (insert (format-time-string "%Y%m%d_%H%M" (current-time))))


(defun mkw/open-directory-in-system-viewer ()
  (interactive)
  (when-system gnu/linux
    (if default-directory
	(browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))
  (when-system windows-nt
    (require 'w32-browser)
    (if default-directory
	(w32explore (expand-file-name default-directory))
      (error "No `default-directory' to open"))))

(defun mkw/insert-global-set-key (key command)
  (interactive (list (read-key-sequence "Key sequence: ")
		     (read-command "Command: ")))
  (prin1 `(global-set-key (kbd ,(key-description key)) ',command)
	 (current-buffer)))

;; (defun insert-global-set-key (key command)
;;   (interactive (list (read-key-sequence "Key sequence: ")
;;		     (read-command "Command: ")))
;;   (prin1 `(global-set-key (kbd ,(key-description key)) ',command)
;;	 (current-buffer)))

(defun mkw/insert-key-sequence (key)
  (interactive (list (read-key-sequence "Key sequence: ")))
  (prin1 `,(key-description key) (current-buffer)))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defmacro when-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; Function for finding out info about font at cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun dwim-backward-kill-word ()
  "DWIM kill characters backward until encountering the beginning of a
word or non-word."
  (interactive)
  (if (thing-at-point 'word) (backward-kill-word 1)
    (let* ((orig-point              (point))
	   (orig-line               (line-number-at-pos))
	   (backward-word-point     (progn (backward-word) (point)))
	   (backward-non-word-point (progn (goto-char orig-point) (backward-non-word) (point)))
	   (min-point               (max backward-word-point backward-non-word-point)))
      (if (< (line-number-at-pos min-point) orig-line) (progn (goto-char min-point) (end-of-line) (delete-horizontal-space))
	(delete-region min-point orig-point)
	(goto-char min-point)))))

(defun backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))

(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
	 (backword)
	 (end)
	 (space-pos)
	 (backword-char (if (bobp)
			    ""           ;; cursor in begin of buffer
			  (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
	(progn
	  (save-excursion
	    (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
	  (setq ab/debug backword)
	  (save-excursion
	    (when (and backword          ;; when backword contains space
		       (s-contains? " " backword))
	      (setq space-pos (ignore-errors (search-backward " ")))))
	  (save-excursion
	    (let* ((pos (ignore-errors (search-backward-regexp "\n")))
		   (substr (when pos (buffer-substring pos cp))))
	      (when (or (and substr (s-blank? (s-trim substr)))
			(s-contains? "\n" backword))
		(setq end pos))))
	  (if end
	      (kill-region cp end)
	    (if space-pos
		(kill-region cp space-pos)
	      (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
	     (while (looking-back "[ \n]")
	       (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

(defun duplicate-thing (comment)
  "Duplicates the current line, or the region if active. If an argument is
given, the duplicated region will be commented out."
  (interactive "P")
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (point-at-bol)))
	  (end   (if (region-active-p) (region-end) (point-at-eol))))
      (goto-char end)
      (unless (region-active-p)
	(newline))
      (insert (buffer-substring start end))
      (when comment (comment-region start end)))))

(defun mkw/standard-drinks (abv quant)
  (interactive(list (read-number "ABV in %")
		    (read-number "How many millilitres?")))
  (print (* abv (/ quant 1000.0) 0.789)))


;; (defun jmi/set-buffer-local-family (font-family)
  ;; "Sets font in current buffer"
  ;; (interactive "sFont Family: ")
  ;; (defface tmp-buffer-local-face
    ;; '((t :family font-family))
    ;; "Temporary buffer-local face")
  ;; (buffer-face-set 'tmp-buffer-local-face))



 (defun my-variable-serif ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Arial" :height 140))
   (buffer-face-mode))

(global-set-key (kbd "<f7> c") 'variable-pitch-mode)

;; (add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t))


;;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my-fill-paragraph (&optional arg)
  "Fill or unfill paragraph.  If repeated, alternate.
A prefix arg for filling means justify (as for `fill-paragraph')."
  (interactive "P")
  (let ((fillp  (not (eq last-command 'fill-paragraph))))
    (apply (setq this-command  (if fillp 'fill-paragraph 'unfill-paragraph))
	   (and fillp  arg  '(full t)))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 5") 'toggle-window-split)


;; This section from https://gitlab.com/fommil/dotfiles/blob/master/.emacs.d/init.el
(defun unfill-paragraph (&optional region)
  ;; http://www.emacswiki.org/emacs/UnfillParagraph
  "Transforms a paragraph in REGION into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun unfill-buffer ()
  "Unfill the buffer for function `visual-line-mode'."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region 0 (point-max))))

(defun revert-buffer-no-confirm ()
  ;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; (defun safe-kill-emacs ()
;;   "Only exit Emacs if this is a small session, otherwise prompt."
;;   (interactive)
;;   (if (daemonp)
;;       ;; intentionally not save-buffers-kill-terminal as it has an
;;       ;; impact on other client sessions.
;;       (delete-frame)
;;     ;; would be better to filter non-hidden buffers
;;     (let ((count-buffers (length (buffer-list))))
;;       (if (< count-buffers 11)
;;	  (save-buffers-kill-emacs)
;;	(message-box "use 'M-x exit'")))))
;; (global-set-key (kbd "C-x C-c") 'safe-kill-emacs)

(defun dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file "~/.emacs.d/init.el"))


(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "<f6>") 'dot-emacs)

;; end fommil/dotfiles/

;; ergomacs

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
Version 2018-04-02T14:38:04-07:00"
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

(global-set-key (kbd "C-c C-\\") 'xah-shrink-whitespaces)

;; (define-key org-mode-map "\C-ck" #'endless/insert-key)
(defun endless/insert-key (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((is-org-mode (derived-mode-p 'org-mode))
         (tag (if is-org-mode
                  "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char (if is-org-mode -8 -6)))))


(provide 'mkw-utilities)
;; End of mkw-utilities.el
