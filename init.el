(require 'package)
(add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/knix/")

(package-initialize)

(defun enable-my-elisp-settings ()
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'evil)
(evil-mode 1)
(evilnc-default-hotkeys)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "," 'evilnc-comment-operator)
(evil-leader/set-key "f" 'ido-find-file)
(evil-leader/set-key "b" 'ido-switch-buffer)

(setq ido-ignore-buffers '("*Completions*" "*Help*" "*Minibuf-0*" "*Minibuf-2*" "*Minibuf-1*" "*Buffer List*" "*Messages*"))
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)

(require 'key-chord)
(key-chord-mode 1)
(require 'auto-complete)
(require 'mic-paren)
(paren-activate)

(require 'knix-smex)

;; Rainbow delimiters in all programming-related files
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Reduce clutter
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ------------------- Settings -------------------- ;;

(require 'knix-ido)

; Allow mouse clicks for emurjinsees
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(key-chord-define evil-normal-state-map "ef" 'eval-defun)

; No splash screen
(setq inhibit-startup-message t)

;; Load mah theme
;; (load-theme 'base16-eighties t)
;; (load-theme 'base16-tomorrow t)
;; (load-theme 'base16-monokai t)
;; (load-theme 'base16-solarized)

;; Scrolling behavior
(setq redisplay-dont-pause t
      scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Backups to home folder, no autosave
(setq backup-directory-alist `(("." . "~/.backup")))
(setq auto-save-default nil)

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; ----------------- Copy / Paste in OSX -------------------- ;;
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; ----------------- Core bindings -------------------- ;;
(define-key evil-insert-state-map (kbd "C-s") (lambda () (interactive) (save-buffer) (evil-normal-state)))
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
;; Map jk -> ESC 
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Meta key
(setq ns-right-alternate-modifier nil)

(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)


;; LaTeX
(require 'smartparens-config)
(add-hook 'latex-mode-hook 'smartparens-mode)
(add-hook 'latex-mode-hook 'flycheck-mode)
(setq TeX-PDF-mode t)

(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)

(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
     (setq ac-sources
	            (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
			                     ac-sources)))

(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)

(require 'knix-paredit)

(require 'knix-clojure)

(require 'knix-ac)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
