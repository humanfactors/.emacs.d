
(setq deft-extensions '("org" "md" "tex" "txt"))
(setq deft-directory "~/Dropbox/Notes")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)

(define-key global-map (kbd "<f5> <f5>") 'deft)

(with-eval-after-load 'deft
  (define-key deft-mode-map (kbd "C-<return>") 'deft-new-file))


;; Deft
(with-eval-after-load 'deft
  (define-key deft-mode-map (kbd "C-<return>") 'deft-new-file)
  (define-key deft-mode-map (kbd "C-c C-n") 'deft-new-filqe-named)
  (define-key deft-mode-map (kbd "C-c C-m") 'deft-new-file-named)
)


(provide 'mdw-deft)
;;; mdw-deft.el ends here

