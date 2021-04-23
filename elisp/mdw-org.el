;; MDW Org Configuration

(require 'org)

(use-package org
  :demand
  :ensure org-plus-contrib
  :config
  (require 'org-tempo)
  (add-hook 'org-mode-hook 'auto-save-mode)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))
  (setf org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (setq org-hierarchical-todo-statistics t)
  (setq org-bullets-mode nil)
  (setq org-indent-mode nil)
  (setq org-hide-leading-stars nil)
  (setq org-support-shift-select t)
  (setq org-cycle-separator-lines 1)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t
        org-edit-src-content-indentation 2)
  (setq org-ellipsis " ▾")

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight 'bold :height 1.5)
  ;; (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'regular :height (cdr face))
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)


  (custom-set-faces '(org-done ((t (:weight normal :strike-through t)))))
  (custom-set-faces '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight bold))))
                    '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
                    '(org-level-3 ((t (:inherit outline-3 :height 1.15))))
                    '(org-level-4 ((t (:inherit outline-4 :height 1.1 :slant italic))))
                    '(org-level-5 ((t (:inherit outline-5 :height 1.0 :slant italic))))
                    '(org-quote ((t (:inherit org-quote :background "#363848")))))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

  ;; Electric pair things for orgmode only
  ;; (electric-pair-mode 1)
  ;; (defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=) (?\_ . ?\_) (?~ . ?~) (?+ . ?+))
  ;;   "Electric pairs for org-mode.")

  ;; (defun org-add-electric-pairs ()
  ;;   (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  ;;   (setq-local electric-pair-text-pairs electric-pair-pairs))
  ;; (add-hook 'org-mode-hook 'org-add-electric-pairs)

  ;; This is it mate
  (setq org-directory "~/Dropbox/org/"
        org-support-shift-select 1
        org-agenda-files (list "~/Dropbox/org/"
                               "~/Dropbox/org/Committees/"))
  (tempo-define-template "title"
                         '("#+TITLE: ?\n" >)
                         "<ti"
                         "Insert a title")

  (tempo-define-template "datetime"
                         '("#+DATE: ?\n" >)
                         "<date"
                         "Insert a title")
  :init
  (defun org-wrap-quote ()
    (interactive)
    (let ((start (min (point) (mark)))
          (end (max (point) (mark))))
      (goto-char end)
      (unless (bolp)
        (newline))
      (insert "#+END_QUOTE\n")
      (goto-char start)
      (unless (bolp)
        (newline))
      (insert "#+BEGIN_QUOTE\n")))

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

  (defun mdw/org-indir-buffer-open-full ()
    (interactive)
    (progn
      (org-tree-to-indirect-buffer)
      (other-window 1)
      (delete-other-windows)))

  (general-define-key
   :keymaps 'org-mode-map
   "C-c b" (mdw|org-emphasize mdw/org-bold ?*)
   "C-c `" (mdw|org-emphasize mdw/org-code ?~)
   "C-c i" (mdw|org-emphasize mdw/org-italic ?/)
   "C-c l" (mdw|org-emphasize mdw/org-literal ?=)
   "<f7> b" 'mdw/org-indir-buffer-open-full
   "<f7> o" 'org-tree-to-indirect-buffer)




(defun unpackaged/org-fix-blank-lines (prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

;;;###autoload
(defmacro unpackaged/def-org-maybe-surround (&rest keys)
  "Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'."
  `(progn
     ,@(cl-loop for key in keys
                for name = (intern (concat "unpackaged/org-maybe-surround-" key))
                for docstring = (format "If region is active, surround it with \"%s\", otherwise call `org-self-insert-command'." key)
                collect `(defun ,name ()
                           ,docstring
                           (interactive)
                           (if (region-active-p)
                               (let ((beg (region-beginning))
                                     (end (region-end)))
                                 (save-excursion
                                   (goto-char end)
                                   (insert ,key)
                                   (goto-char beg)
                                   (insert ,key)))
                             (call-interactively #'org-self-insert-command)))
                collect `(define-key org-mode-map (kbd ,key) #',name))))

(unpackaged/def-org-maybe-surround "~" "=" "*" "/" "+")

(defun unpackaged/org-outline-numbers (&optional remove-p)
  "Add outline number overlays to the current buffer.
When REMOVE-P is non-nil (interactively, with prefix), remove
them.  Overlays are not automatically updated when the outline
structure changes."
  ;; NOTE: This does not necessarily play nicely with org-indent-mode
  ;; or org-bullets, but it probably wouldn't be too hard to fix that.
  (interactive (list current-prefix-arg))
  (cl-labels ((heading-number ()
               (or (when-let ((num (previous-sibling-number)))
                     (1+ num))
                   1))
              (previous-sibling-number ()
               (save-excursion
                 (let ((pos (point)))
                   (org-backward-heading-same-level 1)
                   (when (/= pos (point))
                     (heading-number)))))
              (number-list ()
               (let ((ancestor-numbers (save-excursion
                                         (cl-loop while (org-up-heading-safe)
                                                  collect (heading-number)))))
                 (nreverse (cons (heading-number) ancestor-numbers))))
              (add-overlay ()
               (let* ((ov-length (org-current-level))
                      (ov (make-overlay (point) (+ (point) ov-length)))
                      (ov-string (concat (mapconcat #'number-to-string (number-list) ".")
                                         ".")))
                 (overlay-put ov 'org-outline-numbers t)
                 (overlay-put ov 'display ov-string))))
    (remove-overlays nil nil 'org-outline-numbers t)
    (unless remove-p
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (org-before-first-heading-p)
         (outline-next-heading))
       (cl-loop do (add-overlay)
                while (outline-next-heading))))))

;; End
)

;; (define-key org-mode-map (kbd "<f7> 1") 'unpackaged/org-outline-numbers)







;; '(org-headline-done
;;            ((((class color) (min-colors 16) (background dark))
;;               (:foreground "LightSalmon" :strike-through t)))))



(provide 'mdw-org)
