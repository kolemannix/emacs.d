(defvar package-list
  '(dash cider clojure-mode company evil evil-leader evil-nerd-commenter highlight
	  key-chord paredit smartparens rainbow-delimiters smex)
  "A list of packages to ensure are installed at launch.")

(defun packages-installed-p ()
  (loop for p in package-list 
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'knix-packages)
