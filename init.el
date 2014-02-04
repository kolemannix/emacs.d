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

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
(defun classic-vim-paste () (interactive) (evil-next-line) (evil-paste-after nil))

(define-key evil-normal-state-map (kbd "p") 'classic-vim-paste)

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
(load-theme 'solarized-dark t)

;; Scrolling behavior
(setq redisplay-dont-pause t
      scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Backups to home folder, no autosave
(setq backup-directory-alist `(("." . "~/.backup")))
(setq auto-save-default nil)

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
