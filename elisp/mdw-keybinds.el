
;;; mdw-keybinds.el
;;
;;; Code:

(require 'general)
(require 'undo-tree)



(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-Z") 'undo-tree-redo)

;; Ace Window
(global-set-key (kbd "M-o") 'ace-window)

;; Editing
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-D") 'duplicate-thing)

;; By default, BACKSPACE ON EMACS turns a tab character into a set of spaces
;; & deletes one. This sets backspace to delete 1 character instead of 1 column.
(global-set-key (kbd "DEL") 'backward-delete-char)

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

(defmacro mdw/define-openfile-funconly (filename dir)
  (let ((func (intern (concat "openfile-" filename))))
    `(progn
       (defun ,func ()
         (interactive) (find-file ,dir)))))


(mdw/define-openfile "org" "~/Dropbox/org" "C-x M-o")
(mdw/define-openfile "dropboxmain" "~/Dropbox" "C-x M-1")
(mdw/define-openfile "home" "~/" "C-x M-h")

(mdw/define-openfile-funconly "AHK" "~/Dropbox/Code/AHK")
(mdw/define-openfile-funconly "notes" "~/Dropbox/org")
(mdw/define-openfile-funconly "code" "~/Code/")
(mdw/define-openfile-funconly "emacs-dir" "~/.emacs.d/")

;; Define application keybinds (e.g., deft)
(global-unset-key (kbd "C-\\"))
(general-define-key
 :prefix "C-\\"
 "d" 'deft
 "v" 'mdw/open-directory-in-system-viewer
 )

;; Define file prefix keybinds
(general-define-key
 :prefix "C-\\ o"
 :prefix-command 'opendirs
 "o" 'openfile-org
 "e" 'openfile-emacs-dir
 "d" 'openfile-dropboxmain
 "h" 'openfile-home
 "k" 'openfile-AHK
 "c" 'openfile-code
 "n" 'openfile-notes
 )

;; Define file prefix keybinds
(general-define-key
 :prefix "C-\\ k"
 :prefix-command 'keybind-utilites
 "f" 'free-keys
 "i" 'mdw/insert-global-set-key
 "L" 'describe-bindings
 )

(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture)

(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

;; I hate the emacs backward and forward delte defaults
(global-set-key (kbd "C-<backspace>") 'dwim-backward-kill-word)

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

