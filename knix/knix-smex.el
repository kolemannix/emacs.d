;;; package -- knix-smex.el
;;; Commentary:
;;; Load smex and bind to M-x
;;; Code:
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(provide 'knix-smex)
;;; knix-smex.el ends here

