(add-hook 'prog-mode-hook 'smartparens-strict-mode)

; Bindings
(evil-leader/set-key
  "k" 'sp-kill-sexp
  "b" 'switch-to-buffer
  "s" 'sp-split-sexp)

