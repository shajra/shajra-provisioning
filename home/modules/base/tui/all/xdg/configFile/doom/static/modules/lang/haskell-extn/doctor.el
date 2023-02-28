;;; -*- lexical-binding: t; -*-


(unless
    (or
     (modulep! +lsp)
     (modulep! +dante))
  (warn! (concat
           "You have neither +lsp nor +dante enabled.  At least one of these"
           "\n  features is needed for (:lang haskell-extn) to do anything.")))

(when (modulep! +lsp)
  (unless (modulep! :tools lsp)
    (error! "The +lsp flag of this module requires (:tools lsp)."))
  (unless (modulep! :lang haskell +lsp)
    (error! "The +lsp flag of this module requires (:lang haskell +lsp).")))

(when (modulep! :tools lsp +eglot)
  (error! "This module only supports lsp-mode, not eglot."))
