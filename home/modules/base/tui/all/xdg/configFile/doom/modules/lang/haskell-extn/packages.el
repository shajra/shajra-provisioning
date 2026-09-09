;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "781e4669a0e0917fa8c532371cbfb1eb5b03b645")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "49e39b2bc02014110805f950758716615d96cd3c"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "eed4b8147a1395a3b674577f032321d391cbf19e")
  (package! attrap :pin "22d68a9f8270c5e7d1ab02e01d84f3e45a630a49"))
