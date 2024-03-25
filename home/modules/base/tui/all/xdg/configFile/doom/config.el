;;; -*- lexical-binding: t; -*-


;; Simple settings

;; DESIGN: See how bad this is or isn't on a Mac
;;(let ((scale (if (eq system-type 'darwin) 1.5 1.0)))
  (setq-default
   doom-font                (font-spec :family "@theme_font_mono_code@")
   doom-serif-font          (font-spec :family "@theme_font_mono_serif@")
   doom-unicode-font        (font-spec :family "FreeSerif")  ; alternative to Symbola
   doom-variable-pitch-font (font-spec :family "@theme_font_proportional@")
   )
;;)

;; DESIGN: Recommended by `doom doctor`
(setq-default
 shell-file-name (executable-find "bash")
 vterm-shell (executable-find "fish")
 explicit-shell-file-name (executable-find "fish"))

(setq-default
 dante-methods '(alt-stack-project alt-cabal)
 doom-large-file-size-alist '(("/TAGS\\([.]local\\)?$". 50.0) ("." . 1.0))
 doom-modeline-major-mode-icon t
 doom-theme '@theme_doom_name@
 fancy-splash-image "~/.config/doom/snowman.png"
 fill-column 80
 +haskell-dante-xref-enable nil
 haskell-hoogle-command nil
 lsp-haskell-formatting-provider "stylish-haskell"
 lsp-haskell-server-path "haskell-language-server-wrapper"
 org-agenda-show-all-dates nil
 org-log-into-drawer t
 org-startup-folded 'content
 projectile-project-search-path '(("~/src/work" . 2) ("~/src/shajra" . 2))
 ranger-cleanup-eagerly t
 warning-suppress-types '((with-editor))
 whitespace-line-column 79
 x-select-enable-clipboard (eq system-type 'darwin))

(add-to-list '+org-babel-mode-alist '(fish . shell))

;; DESIGN: render Apple's San Francisco symbols
(set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display")

;; DESIGN: light override of Solarized Light theme
(custom-set-faces!
  `(cursor :background ,(doom-color '@theme_color_unifying@))
  '(term-color-black         :foreground "#@theme_color_normal_black@")
  '(term-color-black         :background "#@theme_color_normal_black@")
  '(term-color-white         :foreground "#@theme_color_normal_white@")
  '(term-color-white         :background "#@theme_color_normal_white@")
  '(term-color-bright-black  :foreground "#@theme_color_bright_black@")
  '(term-color-bright-black  :background "#@theme_color_bright_black@")
  '(term-color-bright-white  :foreground "#@theme_color_bright_white@")
  '(term-color-bright-white  :background "#@theme_color_bright_white@")
  '(ansi-color-black         :foreground "#@theme_color_normal_black@")
  '(ansi-color-black         :background "#@theme_color_normal_black@")
  '(ansi-color-white         :foreground "#@theme_color_normal_white@")
  '(ansi-color-white         :background "#@theme_color_normal_white@")
  '(ansi-color-bright-black  :foreground "#@theme_color_bright_black@")
  '(ansi-color-bright-black  :background "#@theme_color_bright_black@")
  '(ansi-color-bright-white  :foreground "#@theme_color_bright_white@")
  '(ansi-color-bright-white  :background "#@theme_color_bright_white@")
  '(vterm-color-black        :foreground "#@theme_color_normal_black@")
  '(vterm-color-black        :background "#@theme_color_normal_black@")
  '(vterm-color-white        :foreground "#@theme_color_normal_white@")
  '(vterm-color-white        :background "#@theme_color_normal_white@")
  '(vterm-color-bright-black :foreground "#@theme_color_bright_black@")
  '(vterm-color-bright-black :background "#@theme_color_bright_black@")
  '(vterm-color-bright-white :foreground "#@theme_color_bright_white@")
  '(vterm-color-bright-white :background "#@theme_color_bright_white@"))

;; DESIGN: Doom strips down projectile project root discovery for performance
(after! projectile
  (add-to-list 'projectile-project-root-files-bottom-up ".jj"))

;; DESIGN: need to load after Doom to change Doom settings
(after! org-fancy-priorities
  (setq-default
   org-priority-default 4
   org-priority-highest 1
   org-priority-lowest  5
   ;;(?A . "🅐") (?B . "🅑") (?C . "🅒") (?D . "🅓")
   ;;(?A . "🅰") (?B . "🅱") (?C . "🅲") (?D . "🅳")
   ;;(?A . "🄰") (?B . "🄱") (?C . "🄲") (?D . "🄳")
   ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
   ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
   ;;(?1 . " ") (?2 . " ") (?3 . " ") (?4 . " ") (?5 . " ")
   ;;(?1 . "⓵") (?2 . "⓶") (?3 . "⓷") (?4 . "⓸") (?5 . "⓹")
   ;;(?1 . "🌑") (?2 . "🌒") (?3 . "🌓") (?4 . "🌔") (?5 . "🌕")
   ;;(?1 . "🌕") (?2 . "🌔") (?3 . "🌓") (?4 . "🌒") (?5 . "🌑")
   org-fancy-priorities-list
   '((?A . "🅐") (?B . "🅑") (?C . "🅒") (?D . "🅓")
     (?1 . "🌑") (?2 . "🌒") (?3 . "🌓") (?4 . "🌔") (?5 . "🌕"))))


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

;; DESIGN: not indenting by default for long prose, but indenting agenda items
(add-hook! org-load :append
  (setq-default org-startup-indented nil))
(add-hook! org-mode :append
  (if (+org-agenda-files-includes? (buffer-name))
      (org-indent-mode)))


;; Local configuration (some configuration is more personal...)

(load! "managed" "~/.config/doom-local" t)
(load! "unmanaged" "~/.config/doom-local" t)
