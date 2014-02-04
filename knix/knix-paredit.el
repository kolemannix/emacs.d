;;; package -- knix-paredit.el
;;; Commentary:
;;; Load paredit and assign vim-y bindings

;;; Code:
(require 'paredit)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

; Bindings
(global-set-key (kbd "C-k") nil) ; clear up C-k so it can be used as paredit-kill

(evil-leader/set-key
  "w" 'paredit-wrap-round
  "<RET>" 'paredit-close-round-and-newline
  "h" 'paredit-backward-slurp-sexp
  "j" 'paredit-backward-barf-sexp
  "k" 'paredit-forward-barf-sexp
  "l" 'paredit-forward-slurp-sexp
  "S" 'paredit-split-sexp
  "s" 'paredit-splice-sexp
  "x" 'paredit-kill
  )

(provide 'knix-paredit)
;;; knix-paredit.el ends here
