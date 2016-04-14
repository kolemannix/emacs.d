;; Bootstrap
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; ------------------------ Bootstrapped --------------------------- ;;
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/knix/")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;; (use-package dash)

(use-package flx-ido
  :init
  (setq ido-enable-flex-matching t
	ido-ignore-buffers '("*Completions*" "*Help*" "*Minibuf-0*"
			     "*Minibuf-2*" "*Minibuf-1*" "*Buffer List*"
			     "*Echo Area 0*" "*Echo Area 1*" 
			     "*code-conversion-work*" "*scratch*"
			     "*Messages*")
	;; ido-cur-item nil
	;; ido-context-switch-command nil
	;; ido-cur-list nil
	;; ido-default-item nil
	)
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  )

(use-package projectile
  :init
  (setq projectile-use-git-grep t
	projectile-enable-caching t
	projectile-completion-system 'ido)
  :config
  (projectile-global-mode))

(use-package company)

(defun auto-indent-buffer () (interactive) (indent-region (point-min) (point-max)))

(use-package evil
  :init
  (setq evil-want-fine-undo 'fine)
  :config
  (evil-mode 1)
  (diminish 'undo-tree-mode)
  
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "TAB") 'auto-indent-buffer)

  (evil-set-initial-state 'eshell-mode 'insert)
  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (my-move-key evil-motion-state-map evil-normal-state-map " ")

  (define-key evil-insert-state-map (kbd "C-s") (lambda () (interactive)
						  (save-buffer)
						  (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key "," 'evilnc-comment-operator)
    (evil-leader/set-key "f" 'projectile-find-file)
    (evil-leader/set-key "g" 'ido-find-file)
    (evil-leader/set-key "b" 'ido-switch-buffer)
    (evil-leader/set-key "x" 'evil-window-delete))

  (use-package evil-nerd-commenter
    :config
    (evilnc-default-hotkeys)
    (evil-leader/set-key "," 'evilnc-comment-operator))
  )


(use-package flycheck
  :config
  (global-flycheck-mode)
  )

(use-package company
  :config
  (global-company-mode)
  (define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
  (define-key evil-insert-state-map (kbd "C-p") 'company-select-previous))

(use-package restclient
  :commands restclient-mode)

;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :init
;;   (golden-ratio-mode 1)
;;   (setq golden-ratio-auto-scale t))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "sh" 'eshell))

(use-package paredit
  :defer 2
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (evil-leader/set-key
    "{" 'paredit-wrap-curly
    "w" 'paredit-wrap-round
    "[" 'paredit-wrap-square
    "h" 'paredit-backward-slurp-sexp
    "l" 'paredit-forward-slurp-sexp
    "S" 'paredit-split-sexp
    "s" 'paredit-splice-sexp
    "j" 'paredit-splice-sexp-killing-backward
    "k" 'paredit-splice-sexp-killing-forward
    ))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ;; Rainbow delimiters in all programming-related files
  (show-paren-mode))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

;; TOOD needed?
(use-package highlight)

;; Languages and Modes

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojure-mode))
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (message "Init clojure")
  :config
  (message "Config Clojure / hook cider")
  (define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
  (use-package cider
    :pin melpa-stable
    :config
    (message "Config Cider")
    (show-paren-mode 1)
    (turn-on-eldoc-mode)
    (diminish 'eldoc-mode)

    (setq nrepl-hide-special-buffers nil)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-popup-stacktraces nil)
    (setq cider-show-error-buffer nil)

    (setq cider-repl-print-length 100)
    (setq cider-prompt-save-file-on-load nil)

    (setq cider-repl-result-prefix "-> ")
    (setq cider-interactive-eval-result-prefix "-> ")
    (setq cider-words-of-inspiration '(""))

    (define-key evil-normal-state-map (kbd "<SPC>") 'cider-eval-defun-at-point)
    (define-key evil-normal-state-map (kbd "C-c b") 'cider-find-var)
    (define-key evil-normal-state-map (kbd "C-c l") 'cider-load-buffer)
    (define-key evil-normal-state-map (kbd "C-c q") 'cider-quit)
    (define-key evil-normal-state-map (kbd "C-c C-c") 'cider-interrupt)

    (evil-leader/set-key "r" (lambda () (interactive) (cider-switch-to-repl-buffer) (evil-normal-state)))
    (evil-leader/set-key "q" 'cider-quit)

    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    )
  )

;; (use-package dash)

(defun enable-my-elisp-settings ()
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (key-chord-define evil-normal-state-map "ef" 'eval-defun)
  (flycheck-mode))

(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)

;; Appearance
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(set-frame-font "Source Code Pro 10")
(setq sml/no-confirm-load-theme t)
(when window-system (set-frame-size (selected-frame) 200 56))

(use-package solarized-theme)
(use-package smart-mode-line
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/livesafe/jkn/lvsf-backend/" ":LiveSafe:") t)
  (add-to-list 'sml/replacer-regexp-list '(":LiveSafe:/core/" ":Core:") t)
  (add-to-list 'sml/replacer-regexp-list '(":LiveSafe:/common/" ":Common:") t)
  (add-to-list 'sml/replacer-regexp-list '("src/main/scala/com/" "::") t)
  )

(defun switch-to-light-theme () (interactive)
       (load-theme 'solarized-light t)
       (sml/setup))
(defun switch-to-dark-theme () (interactive)
       (load-theme 'solarized-dark t)
       (sml/setup))

(switch-to-dark-theme)
;; (switch-to-light-theme)


;; Scrolling behavior
(setq redisplay-dont-pause t
      scroll-margin 14
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Backups to home folder, no autosave
(setq backup-directory-alist `(("." . "~/.backup")))
(setq auto-save-default nil)

(setq ring-bell-function 'ignore)

;; Scala


(use-package ensime
  :commands ensime ensime-mode
  :init
  (use-package scala-mode2
    :config
    (setq
     ;; scala-indent:indent-value-expression t
     ;; scala-indent:align-forms t
     ;; scala-indent:default-run-on-strategy "operators"
     ;; scala-indent:align-parameters t
     max-lisp-eval-depth 50000
     max-specpdl-size 5000)
    )
  (define-key evil-normal-state-map (kbd "<SPC>") 'ensime-type-at-point)
  (define-key evil-normal-state-map (kbd "<DEL>") 'ensime-pop-find-definition-stack)
  (define-key evil-normal-state-map (kbd "<RET>") 'ensime-edit-definition)
  ;; (define-key evil-insert-state-map (kbd "<RET>") 'newline-and-indent)
  (evil-set-initial-state 'ensime- 'insert)
  (add-hook 'scala-mode-hook 'ensime-mode)
  (add-hook 'ensime-inspector-mode-hook
	    (lambda ()
	      (evil-set-initial-state 'eshell-mode 'insert)
	      ;; (bind-key* "q" 'ensime-close-popup-window)
	      ;; (bind-key* "C-j" 'ensime-inspector-backward-page)
	      ;; (bind-key* "C-k" 'ensime-inspector-forward-page)
	      ))
  )

;; Coq - with Clement's coq-company mode
(add-hook 'coq-mode-hook (lambda ()
			   ;; (interactive)
			   (load-file "~/lib/ProofGeneral/generic/proof-site.el")
			   (require 'proof-site)
			   (setq proof-splash-enable nil)
			   (set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil)
			   (use-package company-coq
			     :config
			     (company-coq-initialize)
			     (define-key evil-normal-state-map
			       (kbd "<SPC>") 'company-coq-proof-goto-point)
			     (setq show-paren-mode nil)
			     )
			   ))

;; JS
(use-package js2-mode
  :mode (("\\.json$" . js2-mode)
	 ("\\.js$" . js2-mode))
  :config 
  (message "Config js2")
  (setq-default js2-allow-rhino-new-expr-initializer nil)
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key t)
  (setq-default js2-mirror-mode nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-auto-indent-p t)
  (setq-default js2-include-rhino-externs nil)
  (setq-default js2-include-gears-externs nil)
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-rebind-eol-bol-keys nil)
  (setq-default js2-include-browser-externs t)
  ;; Let flycheck handle parse errors
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-highlight-undeclared-vars nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

  (use-package js2-highlight-vars)
  )

;; C/C++

(use-package irony 
  :init
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'flycheck-mode))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; Idris
(use-package idris-mode
  :mode ("\\.idr\\'" . idris-mode)
  :init (message "init idris")
  :config
  (message "config idris")
  (idris-define-evil-keys)
  )

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-implicit-gutter-icons t)
 '(ensime-typecheck-idle-interval 0.1)
 '(ensime-typecheck-interval 5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-implicit-highlight ((t nil)))
 )
