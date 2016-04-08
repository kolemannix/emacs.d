; Welcome to my Emacs moving castle

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/knix/")

(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'knix-packages)

(defun enable-my-elisp-settings ()
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)

;; ------------------- Evil Settings -------------------- ;;

(require 'evil)
(evil-mode 1)
(evilnc-default-hotkeys)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "," 'evilnc-comment-operator)
(evil-leader/set-key "f" 'ido-find-file)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "x" 'evil-window-delete)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)

(evil-set-initial-state 'eshell-mode 'insert)

;; ------------------- General -------------------- ;;

(require 'key-chord)
(key-chord-mode 1)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 'knix-smex)

(global-flycheck-mode)

;; Rainbow delimiters in all programming-related files
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(show-paren-mode)

;; Appearance
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(set-frame-font "Source Code Pro 12")
(setq sml/no-confirm-load-theme t)
(when window-system (set-frame-size (selected-frame) 132 55))

(defun switch-to-light-theme () (interactive)
       (load-theme 'solarized-light t)
       (sml/setup))
(defun switch-to-dark-theme () (interactive)
       (load-theme 'solarized-dark t)
       (sml/setup))

(switch-to-dark-theme)
;; (switch-to-light-theme)

(require 'knix-ido)

(key-chord-define evil-normal-state-map "ef" 'eval-defun)
(key-chord-define evil-normal-state-map "sh" 'eshell)

;; Scrolling behavior
(setq redisplay-dont-pause t
      scroll-margin 14
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Backups to home folder, no autosave
(setq backup-directory-alist `(("." . "~/.backup")))
(setq auto-save-default nil)

;; ----------------- Core bindings -------------------- ;;
(define-key evil-insert-state-map (kbd "C-s") (lambda () (interactive)
						(save-buffer)
						(evil-normal-state)))
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)

;; Map jk -> ESC 
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; LaTeX
(setq TeX-PDF-mode t)

;; ----------------- Completion Engine -------------------- ;;

(add-hook 'after-init-hook 'global-company-mode)
(define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
(define-key evil-insert-state-map (kbd "C-p") 'company-select-previous)

;; ----------------- Org Mode -------------------- ;;
(require 'org)

;; ----------------- My Packages -------------------- ;;

(require 'knix-paredit)

;; Clojure
(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
(add-hook 'cider-mode-hook
	  (lambda ()
	    (interactive)
	    (require 'knix-clojure)))

;; Coq
(load-file "~/lib/ProofGeneral/generic/proof-site.el")

;; enable Clement's coq-company mode
(require 'proof-site)
(setq proof-splash-enable nil)
(set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil)
(add-hook 'coq-mode-hook (lambda ()
			   (interactive)
			   (company-coq-initialize)
			   (define-key evil-normal-state-map (kbd "<SPC>")
			     'company-coq-proof-goto-point)
			   (setq show-paren-mode nil)))

;; JS
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (interactive)
	    (require 'knix-js)))


;; C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'flycheck-mode)

;; Python
(require 'knix-python)

;; Idris
(add-hook 'idris-mode-hook
	  (lambda ()
	    (interactive)
	    (add-to-list 'completion-ignored-extensions ".ibc")
	    (idris-define-evil-keys)))

(add-hook 'idris-repl-mode-hook
	  (lambda ()
	    (interactive)
	    (define-key evil-insert-state-map (kbd "C-c <RET>") 'idris-repl-return)
	    (define-key evil-insert-state-map (kbd "<up>") 'idris-repl-backward-history)
	    (define-key evil-insert-state-map (kbd "<down>") 'idris-repl-forward-history)))

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

;;; init.el ends here!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(js2-highlight-external-variables nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "gray29")))))

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))
