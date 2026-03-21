;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "2dd755a5fa11577a9388af88f385d2a8e18f7a8d")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "871a0ef2e98b3a749d0b69d958698000ca5640d3"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "eed4b8147a1395a3b674577f032321d391cbf19e")
  (package! attrap :pin "ad1d9443fcd93e32f2aefadc5af2646701664581"))
