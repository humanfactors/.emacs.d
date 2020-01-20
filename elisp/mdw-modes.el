

;; Markdown Mode
(with-eval-after-load 'markdown-mode


;; Fix inline codeblocks being split in markdown mode in Rmarkdown documents when filling
  (add-hook 'fill-nobreak-predicate
            #'markdown-inline-code-at-point-p)

(setq markdown-italic-underscore t)
(setq markdown-asymmetric-header t)
(setq markdown-list-indent-width 4)

)

(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(provide 'mdw-modes)
