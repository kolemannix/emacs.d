;;; package -- knix-paredit.el
;;; Commentary:
;;; Load paredit and assign vim-y bindings

;;; Code:
(require 'paredit)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(evil-leader/set-key
  "{" 'paredit-wrap-curly
  "w" 'paredit-wrap-round
  "[" 'paredit-wrap-square
  "h" 'paredit-backward-slurp-sexp
  "l" 'paredit-forward-slurp-sexp
  "S" 'paredit-split-sexp
  "s" 'paredit-splice-sexp
  "j" 'paredit-splice-sexp-killing-backward
  "k" 'paredit-splice-sexp-killing-forward
  )

(provide 'knix-paredit)
;;; knix-paredit.el ends here
