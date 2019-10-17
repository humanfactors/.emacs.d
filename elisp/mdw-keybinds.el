
;;; mdw-keybinds.el
;;
;;; Code:

(global-set-key (kbd "M-<f4>") 'kill-emacs)
(global-set-key (kbd "C-z") 'undo)


;; Editing
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

;; Buffer Nav
(global-set-key (kbd "M-<prior>") `previous-buffer)
(global-set-key (kbd "M-<next>") `next-buffer)
(global-set-key (kbd "s-<prior>") `previous-buffer)
(global-set-key (kbd "s-<next>") `next-buffer)

;; Text Nav
(global-set-key (kbd "C-{") 'backward-sentence)
(global-set-key (kbd "C-}") 'forward-sentence)



;; Keybinds for Custom Functions defined in mdw-utilities.el
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key "\C-x\M-s" `michael-filetimestamp)
(global-set-key "\C-x\M-d" `michael-timestamp)

;; Appearance
(define-key global-map (kbd "<f7><f7>") 'visual-fill-column-mode)


;; File Nav

(defmacro mdw/define-openfile (filename dir keybind)
  (let ((func (intern (concat "openfile-" filename))))
    `(progn
       (defun ,func ()
         (interactive) (find-file ,dir))
       (global-set-key (kbd ,keybind) ',func))))


(mdw/define-openfile "dropboxmain" "~/Dropbox" "C-x M-1")
(mdw/define-openfile "home" "~/" "C-x M-h")
(mdw/define-openfile "AHK" "~/Dropbox/Code/AHK" "C-x M-a")
(mdw/define-openfile "org" "~/Dropbox/org" "C-x M-o")
(mdw/define-openfile "notes" "~/Dropbox/org" "C-x M-n")
(mdw/define-openfile "code" "~/Code/" "C-x M-c")

;; M-up and M-down move lines



;; Hydra acewindow

;; (defhydra hydra-window (:color red
;;                         :columns nil)
;;   "window"
;;   ("h" windmove-left nil)
;;   ("j" windmove-down nil)
;;   ("k" windmove-up nil)
;;   ("l" windmove-right nil)
  
  
;;   ("left" windmove-left nil)
;;   ("down" windmove-down nil)
;;   ("up" windmove-up nil)
;;   ("right" windmove-right nil)
  
  
;;   ("H" hydra-move-splitter-left nil)
;;   ("J" hydra-move-splitter-down nil)
;;   ("K" hydra-move-splitter-up nil)
;;   ("L" hydra-move-splitter-right nil)
  
;;   ("|" (lambda ()
;;          (interactive)
;;          (split-window-right)
;;          (windmove-right))
;;        "vert")
;;   ("-" (lambda ()
;;          (interactive)
;;          (split-window-below)
;;          (windmove-down))
;;        "horz")

;;   ("t" transpose-frame "'" :exit t)
;;   ("o" delete-other-windows "one" :exit t)
;;   ("a" ace-window "ace")
;;   ("s" ace-swap-window "swap")
;;   ("d" ace-delete-window "del")
;;   ("m" ace-maximize-window "ace-one" :exit t)
;;   ("q" nil "cancel")
;;   ("f" nil))

;; (global-set-key (kbd "C-M-o") 'hydra-window/body)

(provide 'mdw-keybinds)
;;; mdw-keybinds.el ends here

