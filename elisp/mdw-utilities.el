;;; mdw-utilities.el --- mdw-utilities description
;;
;;; Code:

;; Utility Functionality
(defun michael-timestamp ()
  "Insert a timestamp at the current point.
Note no attempt to go to beginning of line and no added carriage return.
Uses `bjk-timestamp-format' for formatting the date/time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a @ %H:%M" (current-time))))

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
        (goto-char min-point))
      )))

(defun backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))

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

(provide 'mdw-utilities)
;; End of mdw-utilities.el
