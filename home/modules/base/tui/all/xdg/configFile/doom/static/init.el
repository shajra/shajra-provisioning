;;; -*- lexical-binding: t; -*-


(doom!

  :completion
  company             ; the ultimate code completion backend
  vertico             ; the search engine of the future

  :ui
  doom                ; what makes DOOM look the way it does
  doom-dashboard      ; a nifty splash screen for Emacs
  ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
  ;;(emoji +unicode)  ; 🙂
  hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
  indent-guides       ; highlighted indent columns
  (ligatures          ; ligatures and symbols to make your code pretty again
    ;;+extra          ; removed to stremline performance
  )
  modeline            ; snazzy, Atom-inspired modeline, plus API
  nav-flash           ; blink cursor line after big motions
  ophints             ; highlight the region an operation acts on
  (popup +defaults)   ; tame sudden yet inevitable temporary windows
  treemacs            ; a project drawer, like neotree but cooler
  unicode             ; extended unicode support for various languages
  (vc-gutter +pretty) ; vcs diff in the fringe
  vi-tilde-fringe     ; fringe tildes to mark beyond EOB
  window-select       ; visually switch windows
  ;;workspaces        ; tab emulation, persistence & separate workspaces
  zen                 ; distraction-free coding or writing

  :editor
  (evil +everywhere)  ; come to the dark side, we have cookies
  file-templates      ; auto-snippets for empty files
  fold                ; (nigh) universal code folding
  format              ; automated prettiness
  multiple-cursors    ; editing in many places at once
  rotate-text         ; cycle region at point between text candidates
  snippets            ; my elves. They type so I don't have to
  word-wrap           ; soft wrapping with language-aware indent

  :emacs
  dired               ; making dired pretty [functional]
  electric            ; smarter, keyword-based electric-indent
  ibuffer             ; interactive buffer management
  undo                ; persistent, smarter undo for your inevitable mistakes
  vc                  ; version-control and Emacs, sitting in a tree

  :term
  vterm               ; the best terminal emulation in Emacs

  :checkers
  grammar             ; tasing grammar mistake every you make
  (spell              ; tasing you for misspelling mispelling
    +aspell
    +everywhere
  )
  syntax              ; tasing you for every semicolon you forget

  :tools
  emacs-direnv
  docker
  (eval +overlay)   ; run code, run (also, repls)
  gist              ; interacting with github gists
  lookup            ; navigate your code and its documentation
  (lsp +peek)       ; M-x vscode
  magit             ; a git porcelain for Emacs
  make              ; run make tasks from Emacs
  rgb               ; creating color strings
  taskrunner        ; taskrunner for all your projects
  tmux              ; an API for interacting with tmux
  tree-sitter       ; syntax and parsing, sitting in a tree...

  :os
  (:if IS-MAC macos) ; improve compatibility with macOS
  tty                ; improve the terminal Emacs experience

  :lang
  (cc
    ;;+lsp
    +tree-sitter
  )
  ;;coq
  data
  ;;dhall
  emacs-lisp
  ;;(ess +lsp)
  ;;(go +lsp +tree-sitter)
  ;;(graphql +lsp)
  (haskell +lsp)
  ;;(haskell-extn +dante +lsp)
  ;;idris
  (json
    ;;+lsp
    +tree-sitter
  )
  ;;(java +lsp +tree-sitter)
  ;;(javascript +lsp +tree-sitter)
  (latex
    ;;+lsp
  )
  (markdown +grip)
  (nix
    +tree-sitter
  )
  ;;(ocaml +lsp +tree-sitter)
  (org
    ;;+hugo
    +pretty
    ;;+present
  )
  plantuml
  ;;(purescript +lsp)
  (python
    ;;+conda
    ;;+cython
    ;;+lsp
    ;;+poetry
    ;;+pyright
    +tree-sitter
  )
  (racket
    ;;+lsp
    ;;+xp
  )
  (rest +jq)
  ;;(ruby +lsp +tree-sitter)
  (rust
    ;;+lsp
  )
  ;;(scala +lsp +tree-sitter)
  (sh
    +fish
    ;;+lsp
    +tree-sitter
  )
  ;;sml
  (web
    ;;+lsp
    +tree-sitter
  )
  (yaml
    ;;+lsp
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
  ;;twitter           ; twitter client https://twitter.com/vnought

  :config
  dir-locals
  ;;literate
  (default +bindings +smartparens))
