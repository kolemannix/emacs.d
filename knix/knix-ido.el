;;; package -- knix-ido.el
;;; Commentary:
;;; Load ido and tweak settings
;;; Code:
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
; I always fat finger C-x b, so i bind C-x C-b to the same thing
(define-key evil-normal-state-map (kbd "C-x C-b") 'ido-switch-buffer)

(provide 'knix-ido)
;;; knix-ido.el ends here

