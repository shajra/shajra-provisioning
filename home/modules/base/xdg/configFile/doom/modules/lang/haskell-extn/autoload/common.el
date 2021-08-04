;;; -*- lexical-binding: t; -*-
;;;###if (or (featurep! +lsp) (featurep! +dante))


;;;###autoload
(defun +haskell--flycheck-checkers-disable-h ()
  (when (or (eq +haskell-backend 'dante) (eq +haskell-backend 'lsp))
    (dolist (item '(haskell-ghc haskell-stack-ghc))
      (flycheck--toggle-checker item nil)))
  (when (not (eq +haskell-backend 'dante))
    (flycheck--toggle-checker 'haskell-dante nil))
  (when (not (eq +haskell-backend 'lsp))
    (flycheck--toggle-checker 'lsp nil)))

;;;###autoload
(defun +haskell--checkers-disable-h ()
  (add-hook! flycheck-mode #'+haskell--flycheck-checkers-disable-h))

;;;###autoload
(defun +haskell--cond-a (backend orig-fn after-fn &rest args)
  (when (and
         (eq +haskell-backend backend)
         (not (-any?
               (lambda (re) (string-match-p re buffer-file-name))
               +haskell-exclude-regexes)))
    (apply orig-fn args)
    (funcall after-fn)))
