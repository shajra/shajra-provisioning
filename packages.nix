{ nixos
, nixpkgs
, haskell-nix
, hs
}:

{
    prebuilt.nixpkgs = with nixpkgs; {
        inherit
        autojump
        binutils-unwrapped
        cabal2nix
        cabal-install
        cachix
        chromium
        cloc
        coreutils
        dhall
        direnv
        emacs
        escrotum
        feh
        firefoxWrapper
        freemind
        ghc
        gitFull
        global
        gnugrep
        gnumake
        gnupg
        graphviz
        haskell-ci
        htop
        imagemagick
        inkscape
        ispell
        jq
        libreoffice
        mpc_cli
        niv
        nix-index
        oh-my-zsh
        pandoc
        patchelf
        pciutils
        playerctl
        postgresql
        powertop
        procps
        pstree
        ripgrep
        rofi-unwrapped
        rsync
        rxvt_unicode-with-plugins
        sbt-extras
        schemaspy
        shellcheck
        silver-searcher
        slack
        slack-term
        stack
        travis
        tree
        unison
        unzip
        vimHugeX
        wget
        which
        whipper
        xclip
        zathura;
        grip = python38Packages.grip;
        texlive = nixpkgs.texlive.combine {
            inherit (nixpkgs.texlive) scheme-medium;
        };
    };

    # for now everything prebuilt seems found in unstable
    prebuilt.nixos = with nixos; {
        inherit nix-diff;  # marked broken in unstable, 20-3-21
    };

    prebuilt.haskell-nix = with haskell-nix; {
        inherit nix-tools;
    };

    build.nixpkgs = {}
        // (hs.np.fromPackages   "stable" "ghc865" "haskdogs")   # marked broken in unstable, 20-3-21
        // (hs.np.fromPackages   "stable" "ghc865" "pointful")   # marked broken in unstable, 20-3-21
        // (hs.np.fromPackages "unstable" "ghc865" "pointfree")  # broken in ghc883, 20-3-21
        // (hs.np.fromPackages "unstable" "ghc883" "djinn")
        // (hs.np.fromPackages "unstable" "ghc883" "fast-tags")
        // (hs.np.fromPackages "unstable" "ghc883" "ghc-events")
        // (hs.np.fromPackages "unstable" "ghc883" "ghc-events-analyze")
        // (hs.np.fromPackages "unstable" "ghc883" "hasktags")
        // (hs.np.fromPackages "unstable" "ghc883" "hoogle")
        // (hs.np.fromPackages "unstable" "ghc883" "hp2pretty")
        #// (hs.np.fromPackages "unstable" "ghc883" "haskell-code-explorer")  # long build
        #// (hs.np.fromPackages "unstable" "ghc883" "purescript")             # finicky build
        #// (hs.np.fromPackages "unstable" "ghc883" "stack2cabal")            # marked broken, 20-3-21
        #// (hs.np.fromPackages "unstable" "ghc883" "threadscope")            # marked broken, 20-3-21?
        ;

    build.haskell-nix = {}

        #// (hs.hn.fromHackage "ghc883" "apply-refact")                     # missing terminfo, 20-3-21
        #// (hs.hn.fromHackageReinstallableLibGhc "ghc883" "apply-refact")  # time broken, 20-3-21
        #// (hs.hn.fromHackage "ghc865" "apply-refact")                     # missing terminfo, 20-3-21
        // (hs.hn.fromHackageReinstallableLibGhc "ghc865" "apply-refact")

        #// (hs.hn.fromHackage "ghc883" "hlint")                     # missing terminfo, 20-3-21
        #// (hs.hn.fromHackageReinstallableLibGhc "ghc883" "hlint")  # time broken, 20-3-21
        // (hs.hn.fromHackage "ghc865" "hlint")

        #// (hs.hn.fromHackage "ghc883" "stylish-haskell")  # Cabal 3.0.1.0 not on Hackage, 20-3-21
        // (hs.hn.fromHackage "ghc865" "stylish-haskell")

        // (hs.hn.fromHackage "ghc883" "ghcid")

        #// (hs.hn.fromSource  "ghc883" "codex")  # Cabal 3.0.1.0 not on Hackage, 20-3-21
        #// (hs.hn.fromSource  "ghc865" "codex")  # hackage-db broken, 20-3-21
        ;

}
