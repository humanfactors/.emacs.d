;; mdw-complete.el


(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key  "\M-y" 'yank-pop) ; Unrequired fix for crappy popmenu

(with-eval-after-load 'counsel
  (define-key ivy-minibuffer-map (kbd "<left>") 'counsel-up-directory)
  (define-key ivy-minibuffer-map (kbd "M-y") 'ivy-next-line))


(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.3)

(require 'company)
  (setq company-idle-delay              0.3
        company-minimum-prefix-length   3
        company-show-numbers            t
        company-dabbrev-downcase        nil
      )
(add-hook 'after-init-hook 'global-company-mode)


;; No more 
(setq ivy-initial-inputs-alist nil)


(provide 'mdw-complete)
;; end mdw-complete.el
