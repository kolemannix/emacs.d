;; Bootstrap
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure)
(use-package bind-key :ensure)

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

(global-auto-revert-mode t)

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

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-atelier-savanna t))

(defun switch-to-light-theme () (interactive)
       (load-theme 'base16-atelier-savanna-light t)
       (sml/setup))

(defun switch-to-dark-theme () (interactive)
       (load-theme 'base16-atelier-savanna t)
       (sml/setup))
;; (switch-to-dark-theme)

(global-linum-mode)
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))

(use-package evil
  :init
  (setq evil-want-fine-undo 'fine)
  :config
  (evil-mode 1)
  (diminish 'undo-tree-mode)
  
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "C-<tab>") 'ido-switch-buffer)
  (define-key evil-normal-state-map (kbd "TAB") 'auto-indent-buffer)
  (define-key evil-normal-state-map "0" 'evil-first-non-blank)

  ;; For making minor movements without exiting insert mode or reaching for the arrow keys
  (define-key evil-insert-state-map (kbd "C-h") 'left-char)
  (define-key evil-insert-state-map (kbd "C-l") 'right-char)

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
    (evil-leader/set-key "r" 'projectile-replace)
    (evil-leader/set-key "<SPC>" 'smex)
    (evil-leader/set-key "g" 'ido-find-file)
    (evil-leader/set-key "b" 'ido-switch-buffer)
    (evil-leader/set-key "x" 'evil-window-delete))

  (use-package evil-nerd-commenter
    :config
    (evilnc-default-hotkeys)
    (evil-leader/set-key "," 'evilnc-comment-operator))
  )

(add-hook
 'compilation-mode-hook
 (lambda ()
   (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
   (define-key compilation-mode-map (kbd "k") 'compilation-previous-error)
   ;; (setq compilation-scroll-output 'first-error)
   ))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("<f3>" . highlight-symbol)
  :config
  (define-key evil-normal-state-map (kbd "<f4>") 'highlight-symbol-next)
  (define-key evil-normal-state-map (kbd "<f2>") 'highlight-symbol-prev))

(use-package projectile
  :init
  (setq projectile-use-git-grep t
	projectile-enable-caching t
	projectile-completion-system 'ido)
  :config
  (add-hook
   'projectile-mode-hook
   (lambda ()
     ;; (global-unset-key (kbd "C-c c"))
     (evil-define-key 'normal projectile-mode-map (kbd "C-c c") 'projectile-compile-project)
     (evil-define-key 'normal projectile-mode-map (kbd "C-c r") 'projectile-run-project)
     (evil-define-key 'normal projectile-mode-map (kbd "C-c t") 'projectile-test-project)
     (evil-define-key 'normal projectile-mode-map (kbd "C-c f") 'projectile-grep)
     ))
  (projectile-mode)
  )

(setq debug-on-error t)

(use-package flycheck
  :config
  (global-flycheck-mode)
  )

(use-package yasnippet)

(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'lsp-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-map (kbd "<RET>") 'xref-find-definitions)
	      (define-key evil-normal-state-map (kbd "<DEL>") 'xref-pop-marker-stack)
	      ()))
  :custom
  ;; (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (evil-leader/set-key "r" 'lsp-rename)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq
   ;; By setting this to 'nil' we get updates without changing line
   ;; lsp-ui-sideline-update-mode "line"
   lsp-ui-peek-always-show t
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'at-point
   lsp-ui-sideline-update-mode nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-symbol nil 
   lsp-ui-sideline-show-diagnostics t 
   lsp-ui-sideline-delay 0.2
   lsp-ui-sideline-show-code-actions nil 
   lsp-lens-enable t
   lsp-ui-sideline-code-actions-prefix "✏"
   )
  (define-key evil-normal-state-map (kbd "C-c <RET>") 'lsp-execute-code-action)
  (evil-leader/set-key "d" 'lsp-ui-doc-glance)
  )

(use-package company
  :config
  (add-to-list 'completion-styles 'initials t))

;; RUST 
(use-package rustic
  :init
  (define-key evil-normal-state-map (kbd "SPC") 'next-error)
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :config
  (setq rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (add-hook 'rustic-mode-hook 'electric-pair-mode)
  (yas-minor-mode-on)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(defun auto-indent-buffer () (interactive) (indent-region (point-min) (point-max)))
(use-package company
  :config
  (global-company-mode)
  (define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
  (define-key evil-insert-state-map (kbd "C-p") 'company-select-previous))

(use-package restclient
  :commands restclient-mode)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
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

(use-package winum
  :config
  (winum-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ;; Rainbow delimiters in all programming-related files
  (show-paren-mode))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

;; TODO needed?
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
  (key-chord-define evil-normal-state-map "ef" 'eval-defun))

(add-hook 'emacs-lisp-mode-hook 'enable-my-elisp-settings)

;; Appearance
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

(set-face-attribute 'default nil :font "Hack" :height 140)
;; Emoji support
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
;; (set-fontset-font t 'unicode "Symbola" nil 'prepend)

(setq sml/no-confirm-load-theme t)
(when window-system (set-frame-size (selected-frame) 200 56))

;; (😃😇😍)

(use-package solarized-theme)
(use-package smart-mode-line
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/livesafe/jkn/lvsf-backend/" ":LiveSafe:") t)
  (add-to-list 'sml/replacer-regexp-list '(":LiveSafe:/core/" ":Core:") t)
  (add-to-list 'sml/replacer-regexp-list '(":LiveSafe:/common/" ":Common:") t)
  (add-to-list 'sml/replacer-regexp-list '("src/main/scala/com/" "::") t)
  )

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

;; Quick jump to config file
(evil-leader/set-key "`" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Scala

(setq max-specpdl-size 25000)
(setq max-lisp-eval-depth 50000)

;; (setq
;; scala-indent:indent-value-expression t
;; scala-indent:align-forms t
;; scala-indent:default-run-on-strategy "operators"
;; scala-indent:align-parameters t
;; )

(evil-set-initial-state 'eshell-mode 'insert)

;; Coq - with Clement's coq-company mode
;; (load "~/.emacs.d/lisp/PG/generic/proof-site")
;; (use-package company-coq
;;   :config
;;   (company-coq-initialize)
;;   (setq proof-splash-enable nil)
;;   (set-fontset-font t 'greek (font-spec :name "DejaVu Sans Mono") nil)
;;   (define-key evil-normal-state-map
;;     (kbd "<SPC>") 'company-coq-proof-goto-point)
;;   (setq show-paren-mode nil)
;;   )

;; JS
;; (use-package js2-mode
;;   :mode (("\\.json$" . js2-mode)
;; 	 ("\\.js$" . js2-mode))
;;   :config 
;;   (message "Config js2")
;;   (setq-default js2-allow-rhino-new-expr-initializer nil)
;;   (setq-default js2-enter-indents-newline nil)
;;   (setq-default js2-idle-timer-delay 0.1)
;;   (setq-default js2-indent-on-enter-key t)
;;   (setq-default js2-mirror-mode nil)
;;   (setq-default js2-strict-inconsistent-return-warning nil)
;;   (setq-default js2-auto-indent-p t)
;;   (setq-default js2-include-rhino-externs nil)
;;   (setq-default js2-include-gears-externs nil)
;;   (setq-default js2-concat-multiline-strings 'eol)
;;   (setq-default js2-rebind-eol-bol-keys nil)
;;   (setq-default js2-include-browser-externs t)
;;   ;; Let flycheck handle parse errors
;;   (setq-default js2-show-parse-errors nil)
;;   (setq-default js2-highlight-undeclared-vars nil)
;;   (setq-default js2-strict-missing-semi-warning nil)
;;   (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
;; 
;;   (use-package js2-highlight-vars)
;;   )

;; C/C++

;; (use-package irony 
;;   :init
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;; 
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;; 
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; Idris
;; (use-package idris-mode
;;   :mode ("\\.idr\\'" . idris-mode)
;;   :init (message "init idris")
;;   :config
;;   (message "config idris")
;;   (idris-define-evil-keys)
;;   )

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

