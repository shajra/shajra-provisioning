;;; -*- lexical-binding: t; -*-


;; Simple settings

(setq-default
 dante-methods '(alt-stack-project alt-cabal)
 doom-big-font            (font-spec :family "SauceCodePro Nerd Font" :size 18.0)
 doom-font                (font-spec :family "SauceCodePro Nerd Font" :size 11.0)
 doom-large-file-size-alist '(("/TAGS\\([.]local\\)?$". 50.0) ("." . 1.0))
 doom-modeline-major-mode-icon t
 doom-serif-font          (font-spec :family "Source Serif Pro" :size 11.0)
 doom-theme 'doom-solarized-light
 doom-unicode-font        (font-spec :family "FreeSerif")  ; alternative to Symbola
 doom-variable-pitch-font (font-spec :family "Source Serif Variable" :size 11.0)
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

;; DESIGN: this is a hack to deal with the modeline getting too big.  See the
;; code for ‘doom-modeline--font-height’ to get a glimpse at how this is just a
;; hack on top of what is looks like already a hack.
;;
;; I believe that all the settings below in tandem should work fine across
;; different computers and displays.
;;
(custom-set-faces!
  '(mode-line :family "Source Serif Variable" :height 0.9)
  '(mode-line-inactive :family "Source Serif Variable" :height 0.9))
(advice-add #'doom-modeline--font-height :override #'(lambda () (progn 28)))


;; Function calls

(doom/set-indent-width 4)
(global-display-fill-column-indicator-mode)
(after! treemacs (treemacs-follow-mode))


;; Hooks

;; DESIGN: font with Haskell ligatures, but restricted to Haskell code
(add-hook! haskell-mode
  (setq buffer-face-mode-face '(:family "Hasklug Nerd Font"))
  (buffer-face-mode))

;; DESIGN: maybe redundant with the default, but using etags with xref
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

;; DESIGN: Some configuration is more personal...
(load! "config" "/home/tnks/.config/doom-private" t)
