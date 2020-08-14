;; ESS (R and Julia) Configuration
;; Rstudio Style, Emacs Pain.

(use-package ess
  :ensure t
  :defer t
  :commands R julia
  :init
  (require 'ess-site)
  ;; (require 'ess-view)
  ;; (require 'ess-R-data-view)

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

  (defun fm/r()
    "start R with a reasonable layout."
    (interactive)
    ;; Create new window right of the current one
    ;; Current window is 80 characters (columns) wide
    (split-window-right 120)
    ;; Go to next window
    (other-window 1)
    ;; Create new window below current one
    (split-window-below)
    ;; Start R in current window
    (R)
    ;; Go to previous window
    (other-window -1)
    ;; never open any buffer in window with shell
    (set-window-dedicated-p (nth 1 (window-list)) t))


  :config
  (defun ess-eval-word ()
    (interactive)
    (let ((x (ess-edit-word-at-point)))
      (ess-eval-linewise (concat x))))
  (setq comint-move-point-for-output t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq ess-auto-width 'window)
  (setq ess-disable-underscore-assign t)
  (setq ess-eval-visibly 'nowait)
  (setq ess-fancy-comments nil)
  (setq ess-indent-level 2)
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-offset-arguments 'prev-line)
  (setq ess-smart-S-assign-key nil)
  (setq ess-history-directory "~/.emacs-saves/")
  (setq ess-tab-complete-in-script t)
  (setq inferior-R-args "--no-restore-history --no-restore --quiet --no-save")
  (setq tab-width 2)
  (setq ess-style 'RStudio)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-help-own-frame 'one)

  ;; Keybinds for R modes, including custom functions
  :bind(:map ess-r-mode-map
	     ("C-c r" . ess-eval-word)
	     ("C-S-m" . then_R_operator)
	     ("C-'" . tide-insert-assign)
	     ("C-S-<f10>" . inferior-ess-reload)
	     ("C-<return>" . ess-eval-region-or-function-or-paragraph-and-step)
	     ("C-<kp-enter>" . ess-eval-region-or-function-or-paragraph-and-step)
	     ("C-M-<return>" . newline-and-indent))
  :bind(:map inferior-ess-r-mode-map
	     ("C-S-m" . then_R_operator)
	     ("C-S-<f10>" . inferior-ess-reload)
	     ("C-l" . comint-clear-buffer)))

(use-package ess-view
  :ensure t
  :defer t
  :after ess)

(use-package ess-R-data-view
  :defer t
  :after ess)

(defun aj/r-insert-chunk (header)
  "Insert an r-chunk in markdown mode."
  (interactive "sLabel: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

(defun R-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
	(R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))

(global-set-key (kbd "C-x 9") 'R-scratch)

(provide 'mdw-ess)
