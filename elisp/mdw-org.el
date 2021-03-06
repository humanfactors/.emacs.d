;; MDW Org Configuration

(require 'org)

(use-package org
  :defer t
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
   "<f7> o" 'org-tree-to-indirect-buffer))

;; '(org-headline-done
;;            ((((class color) (min-colors 16) (background dark))
;;               (:foreground "LightSalmon" :strike-through t)))))



(provide 'mdw-org)
