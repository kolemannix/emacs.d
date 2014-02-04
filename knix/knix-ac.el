;;; package -- knix-ac.el
;;; Commentary:
;;; Load and configure ac
;;; Code:
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)

(provide 'knix-ac)
;;; knix-ac.el ends here

