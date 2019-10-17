;; Config for ESS
;; ESS Mode (R mode)

(setq ess-tab-complete-in-script t)
(setq ess-indent-with-fancy-comments nil)
(setq ess-disable-underscore-assign t)
(setq ess-smart-S-assign-key nil)
(setq ess-eval-visibly 'nowait)
(setq ess-indent-level 2)
(setq tab-width 2)
(setq ess-fancy-comments nil)


(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

(defun tide-insert-assign ()
  "Insert an assignment <-"
  (interactive)
  (insert "<- "))

;; Mark a word at a point ==============================================
;; http://www.emacswiki.org/emacs/ess-edit.el
(defun ess-edit-word-at-point ()
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

(with-eval-after-load "ess-r-mode"

(defun ess-eval-word ()
  (interactive)
  (let ((x (ess-edit-word-at-point)))
    (ess-eval-linewise (concat x))))

(define-key ess-mode-map  (kbd "C-c r") 'ess-eval-word)
(define-key ess-mode-map  (kbd "C-S-M") 'then_R_operator)
(define-key ess-mode-map  (kbd "C-'") 'tide-insert-assign)

)



(provide 'mdw-ess.el)
