(require 'python-mode)
(evil-set-initial-state 'py-ipython-shell-mode 'insert)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)
; switch to the interpreter after executing code
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

(provide 'knix-python)
