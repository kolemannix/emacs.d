;;; package -- knix-clojure
;;; Commentary:
;;; GG no comminteareeez
;;; Code:


(show-paren-mode 1)

(setq nrepl-hide-special-buffers nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-show-error-buffer nil)

(setq cider-repl-print-length 100)
(setq cider-prompt-save-file-on-load nil)

(setq cider-repl-result-prefix "-> ")
(setq cider-interactive-eval-result-prefix "-> ")
(setq cider-words-of-inspiration '(""))

(define-key evil-normal-state-map (kbd "<SPC>") 'cider-eval-defun-at-point)
(define-key evil-normal-state-map (kbd "C-c b") 'cider-jump-to-var)
(define-key evil-normal-state-map (kbd "C-c l") 'cider-load-buffer)
(define-key evil-normal-state-map (kbd "C-c q") 'cider-quit)

(evil-leader/set-key "r" 'cider-switch-to-repl-buffer)


(defun print-line-in-repl ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-newline ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(global-unset-key (kbd "C-@"))
(define-key evil-normal-state-map (kbd "C-@") 'cider-eval-print-last-sexp)

;; In the repl
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Clojurescript jack-in
(defun insert-cljs-jack-in ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(cemerick.piggieback/cljs-repl :repl-env (cemerick.austin/exec-env))")
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))


(define-key evil-normal-state-map (kbd "C-c C-j s") 'insert-cljs-jack-in)

(provide 'knix-clojure)
;;; knix-clojure.el ends here
