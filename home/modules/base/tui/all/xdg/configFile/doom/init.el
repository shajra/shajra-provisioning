;;; -*- lexical-binding: t; -*-


;; REVISIT: 2026-08-12: REPEATING: Check for new modules to consider
(doom!

  :completion
  (corfu              ; complete with cap(f), cape and a flying feather!
   ;;+dabbrev         ;     extra complexity, just for a fallback
   +icons             ;     trying out icons
   +orderless         ;     recommended default flag
  )
  (vertico            ; the search engine of the future
   ;;+childframe      ;     display completion in child frame (requires GUI)
   +icons             ;     icons to file/buffer completion
  )

  :ui
  dashboard           ; a nifty splash screen for Emacs
  doom                ; what makes DOOM look the way it does
  ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
  (emoji +unicode)    ; 🙂
  hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
  indent-guides       ; highlighted indent columns
  (ligatures          ; ligatures and symbols to make your code pretty again
    +extra            ;     consider removing if performance impacted
  )
  ;;minimap           ; show a map of the code on the side
  (modeline           ; snazzy, Atom-inspired modeline, plus API
   ;;+light           ;     lighter modeline without doom-modeline
  )
  nav-flash           ; blink cursor line after big motions
  ;;neotree           ; [using treemacs] a project drawer, like NERDTree for vim
  ophints             ; highlight the region an operation acts on
  (popup +defaults    ; tame sudden yet inevitable temporary windows
   ;;+all             ;     treat all temporary/special buffers as popups
  )
  ;;smooth-scroll     ; so smooth you won't believe it's not butter
  ;;tabs              ; a tab bar for Emacs
  (treemacs +lsp)     ; a project drawer, like neotree but cooler
  unicode             ; extended unicode support for various languages
  (vc-gutter          ; vcs diff in the fringe
   +diff-hl           ;     possibly better/newer
   +pretty            ;     maybe reasonable improvements
  )
  vi-tilde-fringe     ; fringe tildes to mark beyond EOB
  (window-select      ; visually switch windows
   +numbers           ;     maybe useful
   ;;+switch-window   ;     use switch-window instead of ace-window
  )
  ;;workspaces        ; tab emulation, persistence & separate workspaces
  zen                 ; distraction-free coding or writing

  :editor
  (evil +everywhere)  ; come to the dark side, we have cookies
  file-templates      ; auto-snippets for empty files
  fold                ; (nigh) universal code folding
  (format             ; automated prettiness
   ;;+onsave          ;     reformat buffer on save
   ;;+lsp             ;     use LSP/eglot formatters instead of Apheleia
  )
  ;;god               ; run Emacs commands without modifier keys
  ;;lispy             ; vim for lisp, for people who don't like vim
  multiple-cursors    ; editing in many places at once
  ;;objed             ; text object editing for the innocent
  ;;parinfer          ; turn lisp into python, sort of
  rotate-text         ; cycle region at point between text candidates
  snippets            ; my elves. They type so I don't have to
  (whitespace         ; a butler for your whitespace
   +guess             ;     recommended default flag
   +trim              ;     recommended default flag
  )
  word-wrap           ; soft wrapping with language-aware indent

  :emacs
  (dired              ; making dired pretty [functional]
   +icons             ;     icons are nice
   +dirvish           ;     full dirvish UI (ranger-like)
  )
  electric            ; smarter, keyword-based electric-indent
  ;;eww               ; the internet is gross
  (ibuffer +icons)    ; interactive buffer management
  tramp               ; remote files at your arthritic fingertips
  (undo               ; persistent, smarter undo for your inevitable mistakes
   +tree              ;     undo-tree with visualizer (branching undo)
  )
  vc                  ; version-control and Emacs, sitting in a tree

  :term
  vterm               ; the best terminal emulation in Emacs

  :checkers
  grammar             ; tasing grammar mistake every you make
  (spell              ; tasing you for misspelling mispelling
    +aspell           ;     use aspell backend
    +everywhere       ;     check spelling in programming comments
    ;;+enchant        ;     use enchant-2 backend
    ;;+flyspell       ;     use flyspell instead of spell-fu
    ;;+hunspell       ;     use hunspell backend
  )
  (syntax             ; tasing you for every semicolon you forget
   +icons             ;     unicode icons in error tooltips
   ;;+childframe      ;     display errors in child frame (requires GUI)
   ;;+flymake         ;     use built-in flymake for diagnostics
  )

  :tools
  direnv              ; Doom built-in Direnv integration
  (docker +tree-sitter
   ;;+lsp             ; Dockerfile language server
  )
  ;emacs-direnv       ; an alternate Direnv integration that broke
  (eval +overlay)     ; run code, run (also, repls)
  (lookup             ; navigate your code and its documentation
   +dictionary        ;     may help from needing the browser as much
   +offline           ;     shouldn't take too much space
   ;;+docsets         ;     Dash.app docsets integration
  )
  (lsp +peek          ; M-x vscode
   ;;+booster         ;     speed up LSP I/O via bytecode (Eglot only)
   ;;+eglot           ;     use Eglot instead of LSP-mode
  )
  (magit              ; a git porcelain for Emacs
   ;;+forge           ;     Forge for GitHub issues/PRs (Emacs 29.1+)
  )
  make                ; run make tasks from Emacs
  pdf                 ; pdf enhancements
  (terraform          ;
   ;;+lsp             ;     terraform LSP (terraform-ls or terraform-lsp)
  )
  tmux                ; an API for interacting with tmux
  tree-sitter         ; syntax and parsing, sitting in a tree...

  :os
  (:if (featurep :system 'macos) macos) ; improve compatibility with macOS
  (tty                ; improve the terminal Emacs experience
   +osc               ;     TODO: OSC-52 clipboard via terminal (e.g. over SSH)
  )

  :lang
  (cc +tree-sitter
   ;;+lsp                   ; LSP for C/C++/ObjC (clangd, ccls, cquery)
  )
  ;;coq                     ; no module flags
  data                      ; no module flags
  ;;dhall                   ; no module flags
  emacs-lisp                ; no module flags
  ;;(ess +tree-sitter +lsp)
  (go +tree-sitter
   ;;+lsp                   ; gopls
  )
  ;;(graphql +lsp)
  (haskell +tree-sitter +lsp)
  ;;(haskell-extn +dante +lsp)
  ;;idris
  (java +tree-sitter
   ;;+lsp                   ; eclipse.jdt.ls
  )
  (javascript +tree-sitter
   +lsp                     ; ts-ls, deno-ls
  )
  (json +tree-sitter
   ;;+lsp                   ; vscode-json-languageserver
  )
  (latex +latexmk           ;
   ;;+cdlatex               ; fast math insertion
   ;;+fold                  ; TeX-fold for macros
   ;;+lsp                   ; digestif or TexLab
  )
  (lua +tree-sitter         ;
   ;;+fennel                ; Fennel language support
   ;;+lsp                   ; Lua LSP (EmmyLua, lua-language-server)
   ;;+moonscript            ; Moonscript support
  )
  (markdown +tree-sitter
   +grip                    ; grip for GitHub-style preview
  )
  (nix +tree-sitter +lsp)
  ;;(ocaml +lsp +tree-sitter)
  (org
   +pretty                  ; there are a lot more flags, but they seem frivolous
   ;;+contacts              ; org-contacts
   ;;+crypt                 ; org-crypt
   ;;+dragndrop             ; drag-and-drop images/files
   ;;+gnuplot               ; gnuplot for plots
   ;;+hugo                  ; Hugo export
   ;;+journal               ; org-journal
   ;;+jupyter               ; Jupyter babel
   ;;+noter                 ; org-noter (needs pdf/docview/nov)
   ;;+pandoc                ; pandoc exporter
   ;;+passwords             ; org-passwords
   ;;+pomodoro              ; pomodoro timer
   ;;+present               ; reveal.js/beamer/org-tree-slide
  )
  plantuml                  ; no module flags
  ;;(purescript +lsp)
  (python +tree-sitter
   +lsp                     ; pyright, jedi, ruff, etc.
   +poetry                  ; Poetry env/packaging
   +uv                      ; uv env (not with +pyenv)
   ;;+conda                 ; Conda env support
   ;;+cython                ; Cython support
   ;;+pyenv                 ; pyenv (not with +uv)
   ;;+pyright               ; pyright LSP
  )
  (racket
   ;;+lsp                   ; racket-langserver
   ;;+xp                    ; racket-xp-mode (explore expanded code)
   ;;+hash-lang             ; racket-hash-lang-mode
  )
  (rest +jq)
  (ruby +tree-sitter
   ;;+chruby                ; chruby integration
   ;;+lsp                   ; ruby-lsp or solargraph
   ;;+rails                 ; Rails nav, server, console
   ;;+rbenv                 ; rbenv integration
   ;;+rvm                   ; RVM integration
  )
  (rust +tree-sitter
   ;;+lsp                   ; TODO: rust-analyzer, rls
  )
  (scala +tree-sitter
   +lsp
  )
  (sh +fish +tree-sitter
   ;;+lsp                   ; TODO: bash-language-server
   ;;+powershell            ; TODO: PowerShell syntax (.ps1, .psm1)
  )
  ;;sml                     ; no module flags
  (web +tree-sitter
   ;;+lsp                   ; TODO: web-mode, css-mode LSP
  )
  (yaml +tree-sitter
   ;;+lsp                   ; TODO: yaml-language-server
  )

  :email
  ;;(mu4e +org +gmail)
  ;;notmuch
  ;;(wanderlust +gmail)

  :app
  ;;calendar
  ;;emms
  ;;everywhere        ; *leave* Emacs!? You must be joking
  ;;irc               ; how neckbeards socialize
  ;;(rss +org)        ; emacs as an RSS reader

  :config
  dir-locals
  ;;literate
  (default
   +bindings
   +smartparens
   ;;+gnupg           ; GnuPG integration and pinentry-emacs interop
  ))
