;;; -*- lexical-binding: t; -*-


(unless
    (or
     (featurep! +lsp)
     (featurep! +dante))
  (warn! (concat
           "You have neither +lsp nor +dante enabled.  At least one of these"
           "\n  features is needed for (:lang haskell-extn) to do anything.")))

(when (featurep! +lsp)
  (unless (featurep! :tools lsp)
    (error! "The +lsp flag of this module requires (:tools lsp)."))
  (unless (featurep! :lang haskell +lsp)
    (error! "The +lsp flag of this module requires (:lang haskell +lsp).")))

(when (featurep! :tools lsp +eglot)
  (error! "This module only supports lsp-mode, not eglot."))
