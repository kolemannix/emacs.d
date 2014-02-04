;;; package -- knix-clojure
;;; Commentary:
;;; GG no comminteareeez
;;; Code:

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)

(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)

(define-key evil-normal-state-map (kbd "<SPC>") 'cider-eval-expression-at-point)
(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
(define-key evil-normal-state-map (kbd "C-c b") 'cider-jump)
(define-key evil-normal-state-map (kbd "C-c l") 'cider-load-current-buffer)
(define-key evil-normal-state-map (kbd "C-c q") 'ac-nrepl-popup-doc)
(define-key evil-normal-state-map (kbd "C-c b") 'cider-jump)

(eval-after-load "cider"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; Set up auto-complete
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; highlight expression on eval
(require highlight)
(require 'cider-eval-sexp-fu)
(setq cider-eval-sexp-fu-flash-duration 0.2)

(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)
(provide 'knix-clojure)
;;; knix-clojure.el ends here
