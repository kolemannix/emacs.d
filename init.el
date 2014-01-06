(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/extras/")
(package-initialize)

(defun enable-my-elisp-settings ()
  (turn-on-eldoc-mode)
  (sp-pair "'" nil :actions :rem))
(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)

(require 'evil)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)

(require 'key-chord)
(key-chord-mode 1)
(require 'auto-complete)
(require 'mic-paren)
(paren-activate)

(load-file "~/.emacs.d/extras/smex-config.el")
(load-file "~/.emacs.d/extras/smartparens-config.el")

;; Rainbow delimiters in all programming-related files
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Reduce clutter
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Evil Nerd commenter
(evilnc-default-hotkeys)

;; ------------------- Settings -------------------- ;;

(key-chord-define evil-normal-state-map "ef" 'eval-defun)

; No splash screen
(setq inhibit-startup-message t)


;; Load mah theme
(load-theme 'solarized-dark t)

;; Line numbers, with a space for padding
;; (global-linum-mode t)
;; (setq linum-format "%d ")

; Scrolling behavior
(setq redisplay-dont-pause t
      scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq max-specpdl-size 10000) 
(setq max-lisp-eval-depth 10000)

;; Font lock mode
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

					; Backups to home folder, no autosave
(setq backup-directory-alist `(("." . "~/.backup")))
(setq auto-save-default nil)

;; ----------------- Copy / Paste -------------------- ;;
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; ----------------- Key bindings -------------------- ;;
(define-key evil-insert-state-map (kbd "C-s") (lambda () (interactive) (save-buffer) (evil-normal-state)))
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
;; Map jk -> ESC 
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Meta key
(setq ns-right-alternate-modifier nil)

;; Clojure
; Cider preferences

(define-key evil-normal-state-map (kbd "<SPC>") 'cider-eval-expression-at-point)
(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)

; Cider hooks
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; highlight expression on eval
(require 'highlight)
(require 'cider-eval-sexp-fu)
(setq cider-eval-sexp-fu-flash-duration 0.2)


;; Set up Clojure auto-complete
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
(print "hello")

(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
