;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "2dd755a5fa11577a9388af88f385d2a8e18f7a8d")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "871a0ef2e98b3a749d0b69d958698000ca5640d3"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "ca47f8cc1392c7045db7da8b4fafe86b7c044e90")
  (package! attrap :pin "2cb8d146635a4e1fb92a027551253629c6f55a1b"))
