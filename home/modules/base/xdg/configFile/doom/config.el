;;; -*- lexical-binding: t; -*-


;; Simple settings

(let ((scale (if (eq system-type 'darwin) 1.5 1.0)))
  (setq-default
   doom-big-font            (font-spec :family "SauceCodePro Nerd Font" :size (* scale 18.0))
   doom-font                (font-spec :family "SauceCodePro Nerd Font" :size (* scale 11.0))
   doom-serif-font          (font-spec :family "Source Serif Pro" :size (* scale 11.0))
   doom-unicode-font        (font-spec :family "FreeSerif")  ; alternative to Symbola
   doom-variable-pitch-font (font-spec :family "Source Serif Variable" :size (* scale 11.0))))

(setq-default
 dante-methods '(alt-stack-project alt-cabal)
 doom-large-file-size-alist '(("/TAGS\\([.]local\\)?$". 50.0) ("." . 1.0))
 doom-modeline-major-mode-icon t
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
 warning-suppress-types '((with-editor))
 whitespace-line-column 79
 x-select-enable-clipboard (eq system-type 'darwin))

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

;; DESIGN: light override of Solarized Light theme
;;
(custom-set-faces!
  `(highlight :background ,(doom-color 'magenta))
  `(cursor :background ,(doom-color 'green))
  `(doom-themes-treemacs-file-face :foreground ,(doom-color 'green)))


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

;; DESIGN: Doom dashboard looks bad with an indicator
(add-hook! +doom-dashboard-mode :append
  (display-fill-column-indicator-mode -1))

;; DESIGN: need to load after Doom to change Doom settings
(after! org-fancy-priorities
  (setq-default
   org-priority-default 4
   org-priority-highest 1
   org-priority-lowest  5
   org-fancy-priorities-list '(
                               (?A . "🅐") (?B . "🅑") (?C . "🅒") (?D . "🅓")
                               ;;(?A . "🅰") (?B . "🅱") (?C . "🅲") (?D . "🅳")
                               ;;(?A . "🄰") (?B . "🄱") (?C . "🄲") (?D . "🄳")
                               ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
                               ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
                               ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
                               ;;(?1 . "⓵") (?2 . "⓶") (?3 . "⓷") (?4 . "⓸") (?5 . "⓹")
                               ;;(?1 . "🌑") (?2 . "🌒") (?3 . "🌓") (?4 . "🌔") (?5 . "🌕")
                               (?1 . "🌕") (?2 . "🌔") (?3 . "🌓") (?4 . "🌒") (?5 . "🌑")
                               )))

;; DESIGN: not indenting by default for long prose, but indenting agenda items
(add-hook! org-load :append
  (setq-default org-startup-indented nil))
(add-hook! org-mode :append
  (if (+org-agenda-files-includes? (buffer-name))
      (org-indent-mode)))

;; DESIGN: some configuration is more personal...
(load! "config" "~/.config/doom-private" t)
