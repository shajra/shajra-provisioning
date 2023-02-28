;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "a34ccdc54be15043ff0d253c3c20087524255491")

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "914d4f21252a66fe526abedebe24703bc73397d9")
  (package! attrap :pin "c00f4cc9409dc2fa8c0924f425b2840534b09221"))

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "3249cde75fb411f95fe173c222b848182fd0b752"))
