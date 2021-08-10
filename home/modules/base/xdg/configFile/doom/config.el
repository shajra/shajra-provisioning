;;; -*- lexical-binding: t; -*-


;; Simple settings

(setq-default
 dante-methods '(alt-stack-project alt-cabal)
 doom-large-file-size-alist '(("/TAGS\\([.]local\\)?$". 50.0) ("." . 1.0))
 doom-theme 'doom-solarized-light
 fancy-splash-image "~/.config/doom/snowman.png"
 fill-column 80
 +haskell-dante-xref-enable nil
 haskell-hoogle-command nil
 lsp-haskell-formatting-provider "stylish-haskell"
 lsp-haskell-server-path "haskell-language-server-wrapper"
 org-agenda-show-all-dates nil
 org-log-into-drawer t
 org-startup-folded 'content
 projectile-project-search-path '("~/src/work/" "~/src/shajra/")
 whitespace-line-column 79)

(add-to-list '+org-babel-mode-alist '(fish . shell))


;; Function calls

(doom/set-indent-width 4)
(global-display-fill-column-indicator-mode)
(after! treemacs (treemacs-follow-mode))


;; Hooks

(after! xref
  (add-hook 'xref-backend-functions 'etags--xref-backend))

;; DESIGN: doom dashboard looks bad with an indicator
(add-hook! +doom-dashboard-mode :append
  (display-fill-column-indicator-mode -1))

;; DESIGN: not indenting by default for long prose, but indenting agenda items
(add-hook! org-load :append
  (setq-default org-startup-indented nil))
(add-hook! org-mode :append
  (if (+org-agenda-files-includes? (buffer-name))
      (org-indent-mode)))

;; DESIGN: keepin with Doom Emacs defaults.
;;(add-hook! org-load :append
;;  (setq-default
;;   ;; DESIGN: Emacs default value is nil, which 2-space pads SRC blocks.
;;   ;; Doom Emacs default is t, which puts no padding.
;;   org-src-preserve-indentation nil))

;; DESIGN: Some configuration is more personal...
(load! "config" "/home/tnks/.config/doom-private" t)
