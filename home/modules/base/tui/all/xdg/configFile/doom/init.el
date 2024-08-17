;;; -*- lexical-binding: t; -*-


(doom!

  :completion
  (corfu +orderless)  ; complete with cap(f), cape and a flying feather!
  vertico             ; the search engine of the future

  :ui
  doom                ; what makes DOOM look the way it does
  doom-dashboard      ; a nifty splash screen for Emacs
  ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
  ;;(emoji +unicode)  ; 🙂
  hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
  indent-guides       ; highlighted indent columns
  ;;(ligatures        ; ligatures and symbols to make your code pretty again
  ;;  +extra          ;     consider removing if performance impacted
  ;;)
  modeline            ; snazzy, Atom-inspired modeline, plus API
  nav-flash           ; blink cursor line after big motions
  ophints             ; highlight the region an operation acts on
  (popup +defaults)   ; tame sudden yet inevitable temporary windows
  (treemacs +lsp)     ; a project drawer, like neotree but cooler
  unicode             ; extended unicode support for various languages
  (vc-gutter          ; vcs diff in the fringe
   +diff-hl           ;     possibly better/newer
   +pretty)           ;     maybe reasonable improvements
  vi-tilde-fringe     ; fringe tildes to mark beyond EOB
  (window-select      ; visually switch windows
   +numbers)          ;     maybe useful
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
  (dired              ; making dired pretty [functional]
   +icons +ranger)    ;     icons are nice; we'll see if +ranger feels bulky
  electric            ; smarter, keyword-based electric-indent
  (ibuffer +icons)    ; interactive buffer management
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
  docker            ; +lsp available
  (eval +overlay)   ; run code, run (also, repls)
  (lookup           ; navigate your code and its documentation
   +dictionary      ;     may help from needing the browser as much
   +offline)        ;     shouldn't take too much space
  (lsp +peek)       ; M-x vscode
  magit             ; a git porcelain for Emacs
  make              ; run make tasks from Emacs
  pdf               ; pdf enhancements
  terraform         ; +lsp available
  tmux              ; an API for interacting with tmux
  tree-sitter       ; syntax and parsing, sitting in a tree...

  :os
  (:if (featurep :system 'macos) macos) ; improve compatibility with macOS
  tty                ; improve the terminal Emacs experience

  :lang
  (cc +tree-sitter)         ; +lsp available
  ;;coq                     ; no module flags
  data                      ; no module flags
  ;;dhall                   ; no module flags
  emacs-lisp                ; no module flags
  ;;(ess +tree-sitter +lsp)
  (go +tree-sitter)         ; +lsp available
  ;;(graphql +lsp)
  (haskell +tree-sitter +lsp)
  ;;(haskell-extn +dante +lsp)
  ;;idris
  (java +tree-sitter)       ; +lsp available
  (javascript +tree-sitter) ; +lsp available
  (json +tree-sitter)       ; +lsp available
  (latex +latexmk)          ; +lsp available
  (lua +tree-sitter)        ; +lsp available
  (markdown +grip)
  (nix +tree-sitter +lsp)
  ;;(ocaml +lsp +tree-sitter)
  (org +pretty)             ; there's a lot more flags, but they seem frivolous
  plantuml                  ; no module flags
  ;;(purescript +lsp)
  (python +tree-sitter)     ; +lsp, +poetry, and others available
  (racket)                  ; +lsp and +xp available
  (rest +jq)
  (ruby +tree-sitter)       ; +lsp and others available
  (rust +tree-sitter)       ; +lsp available
  (scala +tree-sitter)      ; +lsp available
  (sh +fish +tree-sitter)   ; +lsp available
  ;;sml                     ; no module flags
  (web +tree-sitter)        ; +lsp available
  (yaml +tree-sitter)       ; +lsp available

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
