;;; package --- Summary
;;; Commentary:
; Welcome to my Emacs moving castle

;;; Code:
(require 'package)
(add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/knix/")

(package-initialize)
(require 'knix-packages)

(defun enable-my-elisp-settings ()
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)

;; ------------------- Evil Bindings -------------------- ;;

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
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
(define-key evil-normal-state-map (kbd "C-w x") 'evil-window-delete)


;; ------------------- General -------------------- ;;

(require 'key-chord)
(key-chord-mode 1)

(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; TODO find out how to disable vc-find-file on save

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
(define-key evil-insert-state-map (kbd "C-s") (lambda () (interactive)
						(save-buffer)
						(evil-normal-state)))
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;; Map jk -> ESC 
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Meta key
(setq ns-right-alternate-modifier nil)

;; LaTeX
(setq TeX-PDF-mode t)

;; ----------------- Completion Engine -------------------- ;;

(add-hook 'after-init-hook 'global-company-mode)
(define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
(define-key evil-insert-state-map (kbd "C-p") 'company-select-previous)

;; ----------------- My Packages -------------------- ;;
					; (require 'knix-go)

(require 'knix-paredit)

(require 'knix-clojure)

;; Temporary convenience fns for graphics class
(defun go-gfx (ray w h rlimit)
 (interactive)
 (shell-command (format "~/gfx/assignments/ray_tracer/go.zsh %s %s %s %s" ray w h rlimit)))

;; COQ
(add-to-list 'load-path "~/.emacs.d/coq/")
(require 'knix-coq)
;;; init.el ends here!
