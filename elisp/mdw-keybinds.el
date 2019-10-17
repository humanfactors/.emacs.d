
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


(mdw/define-openfile-mdub "dropboxmain" "~/Dropbox" "C-x M-1")
(mdw/define-openfile-mdub "home" "~/" "C-x M-h")
(mdw/define-openfile-mdub "AHK" "~/Dropbox/Code/AHK" "C-x M-a")
(mdw/define-openfile-mdub "org" "~/Dropbox/org" "C-x M-o")
(mdw/define-openfile-mdub "notes" "~/Dropbox/org" "C-x M-n")
(mdw/define-openfile-mdub "code" "~/Code/" "C-x M-c")

;; M-up and M-down move lines


(provide 'mdw-keybinds)
;;; mdw-keybinds.el ends here

