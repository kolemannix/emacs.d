;;; package -- knix-clojure
;;; Commentary:
;;; GG no comminteareeez
;;; Code:

(show-paren-mode 1)

(setq nrepl-hide-special-buffers nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
; quiet errors
(setq cider-popup-stacktraces nil)
(setq cider-show-error-buffer nil)

(setq cider-repl-print-length 100)
(setq cider-prompt-save-file-on-load nil)

(setq cider-repl-result-prefix "-> ")
(setq cider-interactive-eval-result-prefix "-> ")

(define-key evil-normal-state-map (kbd "<SPC>") 'cider-eval-defun-at-point)
(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
(define-key evil-normal-state-map (kbd "C-c b") 'cider-jump)
(define-key evil-normal-state-map (kbd "C-c l") 'cider-load-current-buffer)
(define-key evil-normal-state-map (kbd "C-c q") 'cider-quit)


(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(defun print-line-in-repl ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(define-key evil-normal-state-map (kbd "C-c RET") 'print-line-in-repl)

;; In the repl
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; highlight expression on eval
(require 'highlight)
(require 'cider-eval-sexp-fu)
(setq cider-eval-sexp-fu-flash-duration 0.2)


;; Clojurescript jack-in
(defun insert-cljs-jack-in ()
  (interactive)
  (insert "(cemerick.piggieback/cljs-repl :repl-env (cemerick.austin/exec-env))"))

(define-key evil-normal-state-map (kbd "C-c C-j s") 'insert-cljs-jack-in)

(provide 'knix-clojure)
;;; knix-clojure.el ends here
