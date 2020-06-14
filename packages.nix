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
        mpc_cli
        niv
        nix-diff
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

    # DESIGN: occaisionally unstable is actually unstable
    prebuilt.nixos = with nixos; {
        inherit
        libreoffice  # didn't build in unstable, 20-6-6
        ;
    };

    prebuilt.haskell-nix = with haskell-nix; {
        inherit nix-tools;
    };

    build.nixpkgs = {}

        // (hs.np.fromPackages "unstable" "ghc883" "apply-refact")
        // (hs.np.fromPackages "unstable" "ghc883" "djinn")
        // (hs.np.fromPackages "unstable" "ghc883" "fast-tags")
        // (hs.np.fromPackages "unstable" "ghc883" "ghc-events")
        // (hs.np.fromPackages "unstable" "ghc883" "haskdogs")
        // (hs.np.fromPackages "unstable" "ghc883" "hasktags")
        // (hs.np.fromPackages "unstable" "ghc883" "hoogle")
        // (hs.np.fromPackages "unstable" "ghc883" "hp2pretty")
        // (hs.np.fromPackages "unstable" "ghc883" "threadscope")

        // (hs.np.fromPackages "unstable" "ghc865" "pointfree")  # broken in ghc883, 20-6-6

        #// (hs.np.fromPackages "unstable" "ghc883" "pointful")            # marked broken, 20-6-6
        #// (hs.np.fromPackages "unstable" "ghc883" "ghc-events-analyze")  # marked broken, 20-6-6
        ;

    build.haskell-nix = {}
        // (hs.hn.fromHackage "ghc883" "ghcid")
        // (hs.hn.fromHackage "ghc883" "hlint")
        // (hs.hn.fromHackage "ghc883" "stylish-haskell")
        // (hs.hn.fromSource  "ghc883" "codex")
        ;

}
