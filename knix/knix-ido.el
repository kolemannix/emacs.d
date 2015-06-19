;;; package -- knix-ido.el
;;; Commentary:
;;; Load ido and tweak settings
;;; Code:
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-ignore-buffers '("*Completions*" "*Help*" "*Minibuf-0*"
			   "*Minibuf-2*" "*Minibuf-1*" "*Buffer List*"
			   "*Messages*"))

(provide 'knix-ido)
;;; knix-ido.el ends here

