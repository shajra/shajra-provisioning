;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "1a285fc4c50ca74bb5cd9b2a8c1a46a64a77384a")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "6981f8d1225c038c1a130e8cf70530cfe15f976e"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "ca47f8cc1392c7045db7da8b4fafe86b7c044e90")
  (package! attrap :pin "bb61a4bc3d85a76e807f1ecede17031b51c8caed"))
