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



;; Function for finding out info about font at cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))



(provide 'mdw-utilities)
;; End of mdw-utilities.el
