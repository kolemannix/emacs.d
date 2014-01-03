(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)
(require 'key-chord)
(key-chord-mode 1)

;; Evil Nerd commenter
(evilnc-default-hotkeys)

;; ------------------- Settings -------------------- ;;

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

(key-chord-define evil-normal-state-map ",x" 'execute-extended-command)

;; Load mah theme
(load-theme 'solarized-dark t)

;; Line numbers, with a space for padding
(global-linum-mode t)
(setq linum-format "%d ")

;; Meta key
(setq ns-right-alternate-modifier nil)

;; esc as a viable panic button
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*mpletions*") (delete-windows-on "*mpletions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
