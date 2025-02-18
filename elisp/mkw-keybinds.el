;;; mkw-keybinds.el
;;
;;; Code:

(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)

(defun delete-frame-or-kill-emacs ()
  "Delete the selected frame.  If the last one, kill Emacs."
  (interactive)
  (condition-case nil (delete-frame) (error (save-buffers-kill-emacs))))

(global-set-key (kbd "M-<f4>") 'delete-frame-or-kill-emacs)

;; Editing
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-D") 'duplicate-thing)


;;dired
(add-hook
 'dired-mode-hook
 (lambda()
   (define-key dired-mode-map "j" 'ido-find-file)))

;; Yanks and shit
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key (kbd "M-S-Y") 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)
(global-set-key (kbd "C-S-V") 'yank-pop-forwards)
(global-set-key (kbd "C-v") 'yank)

;; I hate the emacs backward and forward delte defaults
(global-set-key (kbd "C-<backspace>") 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)

;; By default, BACKSPACE ON EMACS turns a tab character into a set of spaces
;; & deletes one. This sets backspace to delete 1 character instead of 1 column.
(global-set-key (kbd "DEL") 'backward-delete-char)


;; Buffer Nav
(global-set-key (kbd "M-<prior>") `previous-buffer)
(global-set-key (kbd "M-<next>") `next-buffer)
(global-set-key (kbd "s-<prior>") `previous-buffer)
(global-set-key (kbd "s-<next>") `next-buffer)

;; Text Nav
(global-set-key (kbd "C-{") 'backward-sentence)
(global-set-key (kbd "C-}") 'forward-sentence)

;; Keybinds for Custom Functions defined in mkw-utilities.el
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key "\C-x\M-s" `micah-filetimestamp)
(global-set-key "\C-x\M-d" `micah-timestamp)

;; Appearance
(define-key global-map (kbd "<f7><f7>") 'visual-fill-column-mode)
(define-key global-map (kbd "<f7> n") 'display-line-numbers-mode)

(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)

;; Avy
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)

;; Recent files
(global-set-key (kbd "C-c r") 'counsel-recentf)

;; File Nav
(defmacro mkw/define-openfile (filename dir keybind)
  (let ((func (intern (concat "openfile-" filename))))
    `(progn
       (defun ,func ()
   (interactive) (find-file ,dir))
       (global-set-key (kbd ,keybind) ',func))))

(defmacro mkw/define-openfile-funconly (filename dir)
  (let ((func (intern (concat "openfile-" filename))))
    `(progn
       (defun ,func ()
   (interactive) (find-file ,dir)))))

(defmacro me/define-openfile-funconly (filename dir)
  (let ((func (intern (concat "openfile-" filename))))
    `(defun ,func () (interactive) (find-file ,dir))))

(defmacro me/define-openfiles (files)
  `(progn
     ,@(mapcar (lambda (item)
     `(me/define-openfile-funconly ,(car item) ,(cadr item)))
         files)))

(if (string= (system-name) "NOTHINGUNDONE")
    (progn
      (me/define-openfiles
       (("manuscripts" "d:/Dropbox/org/Manuscripts.org")
        ("AHK" "d:/Dropbox/Code/AHK")
        ("notes" "d:/Dropbox/notes")
        ("code" "~/Code/")
        ("emacs-dir" "~/.emacs.d/")))
      (mkw/define-openfile "org" "d:/Dropbox/org" "C-x M-o")
      (mkw/define-openfile "dropboxmain" "d:/Dropbox" "C-x M-1")
      (mkw/define-openfile "home" "~/" "C-x M-h"))
  nil)

(mkw/define-openfile-funconly "keybinds" "~/.emacs.d/elisp/mkw-keybinds.el")
(mkw/define-openfile-funconly "manuscripts" "~/Dropbox/org/Manuscripts.org")
(mkw/define-openfile-funconly "forrest" "~/Dropbox/org/Forrest.org")
(mkw/define-openfile-funconly "AHK" "~/Dropbox/Code/AHK")
(mkw/define-openfile-funconly "notes" "~/Dropbox/org")
(mkw/define-openfile-funconly "code" "~/Code/")
(mkw/define-openfile-funconly "emacs-dir" "~/.emacs.d/elisp/")
(mkw/define-openfile "org" "~/Dropbox/org/" "C-x M-o")
(mkw/define-openfile "dropboxmain" "~/Dropbox/" "C-x M-1")
(mkw/define-openfile "home" "~/" "C-x M-h")



;; Define application keybinds (e.g., deft)

(global-unset-key (kbd "C-\\"))

(general-define-key
 :keymaps '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-\\"
 "d" 'deft
 "v" 'mkw/open-directory-in-system-viewer
 "n" 'neotree-toggle
 "b" 'evil-buffer-new
 "RET" 'switch-to-dashboard
 )

(general-define-key
 :keymaps '(normal insert emacs)
 :prefix "SPC h"
 :non-normal-prefix "C-\\ h"
 :prefix-command 'help-utils
 "e" 'elisp-index-search
 "f" 'find-function
 "i" 'read-command
)

(general-define-key
 :keymaps '(normal insert emacs)
 :prefix "SPC h"
 :non-normal-prefix "C-\\ h"
 :prefix-command 'elisp-help
 "e" 'elisp-index-search
 "f" 'find-function
 "i" 'read-command
 )

;; Define file prefix keybinds
(general-define-key
 :keymaps '(normal insert emacs)
 :prefix "SPC o"
 :non-normal-prefix "C-\\ o"
 :prefix-command 'opendirs
 "o" 'openfile-org
 "e" 'openfile-emacs-dir
 "d" 'openfile-dropboxmain
 "h" 'openfile-home
 "k" 'openfile-AHK
 "c" 'openfile-code
 "n" 'openfile-notes
 "m" 'openfile-manuscripts
 "f" 'openfile-forrest
 )


;; Define file prefix keybinds
(general-define-key
  :keymaps '(normal insert emacs)
 :prefix "SPC k"
 :non-normal-prefix "C-\\ k"
 :prefix-command 'keybind-utilites
 "f" 'free-keys
 "i" 'mkw/insert-global-set-key
 "L" 'describe-bindings
 )

;; Define file prefix keybinds
(general-define-key
 :prefix "<f5>"
 :prefix-command 'keybinds-notes
 "<f5>" 'deft
 "m" 'openfile-manuscripts
 "f" 'openfile-forrest
 "n" '((lambda () (interactive) (ispell-change-dictionary "nl"))
       :which-key "nederlands")
 "e" '((lambda () (interactive) (ispell-change-dictionary "en_US"))
       :which-key "engels")
 "g" '((lambda () (interactive) (ispell-change-dictionary "en_GB"))
       :which-key "engels_brit")

 "k" 'openfile-keybinds
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

;; New buffer thing

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

(provide 'mkw-keybinds)
;;; mkw-keybinds.el ends here
