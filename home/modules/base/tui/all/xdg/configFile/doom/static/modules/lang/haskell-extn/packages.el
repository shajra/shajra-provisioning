;;; -*- no-byte-compile: t; lexical-binding: t; -*-


(load! "modules/lang/haskell/packages" doom-emacs-dir)

;; DESIGN: match Doom
(package! haskell-mode :pin "3e146c1a89db257bb75c7b33fa2a5a1a85aabd51")

(when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  ;; DESIGN: match Doom
  (package! lsp-haskell :pin "918ffa2516a59c90f909b584f7c9968716c0e006"))

(when (modulep! +dante)
  ;; DESIGN: target latest for both
  (package! dante :pin "ca47f8cc1392c7045db7da8b4fafe86b7c044e90")
  (package! attrap :pin "bb61a4bc3d85a76e807f1ecede17031b51c8caed"))
