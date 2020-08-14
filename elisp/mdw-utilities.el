;;; mdw-utilities.el --- mdw-utilities description
;;
;;; Code:



;; Utility Functionality
(defun michael-timestamp ()
  "Insert a timestamp at the current point.
Note no attempt to go to beginning of line and no added carriage return.
Uses `bjk-timestamp-format' for formatting the date/time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M" (current-time))))

(defun michael-filetimestamp ()
  "Insert a timestamp at the current point.
Note no attempt to go to beginning of line and no added carriage return.
Uses `bjk-timestamp-format' for formatting the date/time."
  (interactive)
  (insert (format-time-string "%Y%m%d_%H%M" (current-time))))


(defun mdw/open-directory-in-system-viewer ()
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

(defun mdw/insert-global-set-key (key command)
  (interactive (list (read-key-sequence "Key sequence: ")
		     (read-command "Command: ")))
  (prin1 `(global-set-key (kbd ,(key-description key)) ',command)
	 (current-buffer)))


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

(defun spacemacs/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun spacemacs/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))


;; from @bmag
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
	     (current-split-vertical-p (car window-tree))
	     (first-window (nth 2 window-tree))
	     (second-window (nth 3 window-tree))
	     (second-window-state (window-state-get second-window))
	     (splitter (if current-split-vertical-p
			   #'split-window-horizontally
			 #'split-window-vertically)))
	(delete-other-windows first-window)
	;; `window-state-put' also re-selects the window if needed, so we don't
	;; need to call `select-window'
	(window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((window-tree (car (window-tree)))
	   (current-split-vertical-p (car window-tree))
	   (first-window (nth 2 window-tree))
	   (second-window (nth 3 window-tree))
	   (second-window-state (window-state-get second-window))
	   (splitter (if current-split-vertical-p
			 #'split-window-horizontally
		       #'split-window-vertically)))
      (delete-other-windows first-window)
      ;; `window-state-put' also re-selects the window if needed, so we don't
      ;; need to call `select-window'
      (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))


(defun insert-global-set-key (key command)
  (interactive (list (read-key-sequence "Key sequence: ")
		     (read-command "Command: ")))
  (prin1 `(global-set-key (kbd ,(key-description key)) ',command)
	 (current-buffer)))

(defun mdw/standard-drinks (abv quant)
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
   (setq buffer-face-mode-face '(:family "Open Sans" :height 140))
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

(provide 'mdw-utilities)
;; End of mdw-utilities.el
