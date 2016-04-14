


(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(define-key evil-normal-state-map (kbd "<SPC>") 'skewer-eval-defun)
(define-key evil-normal-state-map (kbd "C-c l") 'skewer-load-buffer)


;; (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; (local-set-key "\C-cl" 'js-load-file-and-go)


(provide 'knix-js)
