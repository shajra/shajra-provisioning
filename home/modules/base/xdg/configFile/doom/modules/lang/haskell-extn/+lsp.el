;;; -*- lexical-binding: t; -*-


(load! "modules/lang/haskell/+lsp" doom-emacs-dir)

(add-hook! haskell-literate-mode #'lsp!)

(after! (lsp-mode envrc)
  (defadvice! +haskell--lsp-direnv-a (&rest _)
    :before #'lsp
    (envrc-reload)))

(after! (lsp-mode direnv)
  (defadvice! +haskell--lsp-direnv-a (&rest _)
    :before #'lsp
    (direnv-update-directory-environment)))

(after! lsp-mode
  (defadvice! +haskell--cond-lsp-a (orig-fn &rest args)
    :around #'lsp
    (+haskell--cond-a 'lsp orig-fn (lambda ()) args)))
