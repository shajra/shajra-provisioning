{ nixpkgs-stable
, nixpkgs-unstable
, haskell-nix
, hs
}:

let

    nixpkgsDarwin = with nixpkgs-stable; {
        inherit
        nodejs-10_x
        autojump
        bzip2
        cabal-install
        cabal2nix
        cachix
        cloc
        coreutils
        dfu-programmer
        dfu-util
        dhall
        direnv
        fswatch
        ghc
        git
        global
        gnugrep
        gnupg
        graphviz
        hlint
        htop
        imagemagick
        ispell
        jq
        jre
        mpc_cli
        mongodb
        mongodb-tools
        nix-diff
        nix-index
        oh-my-zsh
        pandoc
        patchelf
        postgresql
        procps
        ripgrep
        rsync
        sbt-extras
        schemaspy
        shellcheck
        silver-searcher
        sqlint
        teensy-loader-cli
        tree
        unison
        unzip
        vim
        wget
        ;
    };

    nixpkgsLinux = with nixpkgs-unstable; {
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

in

{
    prebuilt.nixpkgs =
        if builtins.currentSystem == "x86_64-darwin"
        then nixpkgsDarwin
        else nixpkgsLinux;

    prebuilt.haskell-nix = with haskell-nix; {
        inherit nix-tools;
    };

    build.nixpkgs = {}

        // (hs.np.fromPackages "unstable" "ghc883" "djinn")
        // (hs.np.fromPackages "unstable" "ghc883" "fast-tags")
        // (hs.np.fromPackages "unstable" "ghc883" "ghc-events")
        // (hs.np.fromPackages "unstable" "ghc883" "haskdogs")
        // (hs.np.fromPackages "unstable" "ghc883" "hasktags")
        // (hs.np.fromPackages "unstable" "ghc883" "hoogle")
        // (hs.np.fromPackages "unstable" "ghc883" "hp2pretty")
        // (hs.np.fromPackages "unstable" "ghc883" "threadscope")

        // (hs.np.fromPackages "unstable" "ghc865" "pointfree")  # broken for 8.8.3 & 8.10.1, 20-6-14

        #// (hs.np.fromPackages "unstable" "ghc883" "pointful")            # marked broken, 20-6-14
        #// (hs.np.fromPackages "unstable" "ghc883" "ghc-events-analyze")  # marked broken, 20-6-14
        ;

    build.haskell-nix = {}
        // (hs.hn.fromHackage "ghc8101" "apply-refact")
        // (hs.hn.fromHackage "ghc8101" "ghcid")
        // (hs.hn.fromHackage "ghc8101" "hlint")
        // (hs.hn.fromHackage "ghc8101" "stylish-haskell")
        // (hs.hn.fromSource  "ghc8101" "codex")
        ;

}
