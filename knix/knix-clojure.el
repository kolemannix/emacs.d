;;; package -- knix-clojure


;; Clojurescript jack-in
(defun insert-cljs-jack-in ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(cemerick.piggieback/cljs-repl :repl-env (cemerick.austin/exec-env))")
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(define-key evil-normal-state-map (kbd "C-c C-j s") 'insert-cljs-jack-in)

(provide 'knix-clojure)
;;; knix-clojure.el ends here
