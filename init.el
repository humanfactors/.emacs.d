;;; init.el --- startpoint of Michael's Emacs config
;;
;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun time-since-init (load-file)
  (let ((elapsed (float-time (time-subtract (current-time) before-init-time))))
        (message "loading %s...done (%.3fs)" load-file elapsed)))


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
(time-since-init "mdw-packages")
(require 'mdw-defaults)	 ; In theory this should work on almost any emacs config
(time-since-init "mdw-defaults")
(require 'mdw-vibes)	 ; Appearance and fonts.. Ya know, vibes?
(time-since-init "mdw-vibes")
(require 'mdw-utilities) ; Always ensure utilities loads before keybinds
(time-since-init "mdw-utilities")
(require 'mdw-modes)	 ; Always ensure modes loads before keybinds
(time-since-init "mdw-modes")
(require 'mdw-org)       ; Orgmode customisations
(time-since-init "mdw-org")
(require 'mdw-ess)       ; R etc
(time-since-init "mdw-ess")
(require 'mdw-spacemacs) ; Spacemacs ganking
(time-since-init "mdw-spacemacs")
(require 'mdw-keybinds)	 ; Customisations that aren't in use-package definitions
(time-since-init "mdw-keybinds")

