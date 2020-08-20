;;; init.el --- startpoint of Michael's Emacs config
;;
;;; Code:

;; Setup custom file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs.d/emacs-custom.el")

;; This just stops this erroring the first time you load emacs
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (insert ";; emacs-custom.el")
    (write-file custom-file nil)))

(load custom-file)

;; Default directory
(setq default-directory "~/")

;; Ensure my custom Elisp is on loadpath
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/deps")

;; The Mdubziverse
(message "Loading Config")
(require 'mdw-packages)  ; Kind of redundant with use-package... So should fix this
(require 'mdw-defaults)	 ; In theory this should work on almost any emacs config
(require 'mdw-vibes)	 ; Appearance and fonts.. Ya know, vibes?
(require 'mdw-utilities) ; Always ensure utilities loads before keybinds
(require 'mdw-modes)	 ; Always ensure modes loads before keybinds
(require 'mdw-ess)	 ; R etc
(require 'mdw-org)	 ; Orgmode customisations
(require 'mdw-spacemacs) ; Spacemacs ganking
(require 'mdw-keybinds)	 ; Customisations that aren't in use-package definitions
