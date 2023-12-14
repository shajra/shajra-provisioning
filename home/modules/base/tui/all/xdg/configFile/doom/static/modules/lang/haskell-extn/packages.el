;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "79eaf444a72109f93f552abb53f834cc63bbf9f2")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "89d16370434e9a247e95b8b701f524f5abfc884b"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "ca47f8cc1392c7045db7da8b4fafe86b7c044e90")
  (package! attrap :pin "bb61a4bc3d85a76e807f1ecede17031b51c8caed"))
