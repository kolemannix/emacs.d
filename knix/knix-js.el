

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-enter-indents-newline nil)
;; (setq-default js2-global-externs '("print" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key t)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)


(setq-default js2-include-browser-externs t)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-highlight-undeclared-vars nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(define-key evil-normal-state-map (kbd "<SPC>") 'skewer-eval-defun)
(define-key evil-normal-state-map (kbd "C-c l") 'skewer-load-buffer)


;; (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; (local-set-key "\C-cl" 'js-load-file-and-go)


(provide 'knix-js)
