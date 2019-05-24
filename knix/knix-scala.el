;;; package -- knix-scala

(use-package scala-mode :mode "\\.s\\(cala\\|bt\\)$")
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (add-hook 'scala-mode-hook 'electric-pair-mode)
  (add-to-list 'company-backends 'company-capf)
  )

(use-package lsp-scala
  :after scala-mode
  :demand t
  ;; Optional - enable lsp-scala automatically in scala files
  :hook (scala-mode . lsp))
;; (add-hook 'ensime-inspector-mode-hook
;; 	    (lambda ()
;; (bind-key* "q" 'ensime-close-popup-window)
;; (bind-key* "C-j" 'ensime-inspector-backward-page)
;; (bind-key* "C-k" 'ensime-inspector-forward-page)
;; ))

(provide 'knix-scala)
;;; knix-scala.el ends here
