{ np
, hn
, isDarwin
}:

let

    pick = np.pick (if isDarwin then "stable" else "unstable");
    pickIfDarwin = if isDarwin then pick else (attrNames: {});
    pickIfLinux = if isDarwin then (attrNames: {}) else pick;

    nixpkgs.common.prebuilt = pick [
        "autojump"
        "bzip2"
        "cabal2nix"
        "cabal-install"
        "cachix"
        "cloc"
        "coreutils"
        "dhall"
        "direnv"
        "ghc"
        "gitFull"
        "gnugrep"
        "gnumake"
        "gnupg"
        "graphviz"
        "htop"
        "imagemagick"
        "ispell"
        "jq"
        "jre"
        "mpc_cli"
        "niv"
        "nix-diff"
        "nix-index"
        "oh-my-zsh"
        "pandoc"
        "patchelf"
        "procps"
        "pstree"
        "python38Packages.grip"
        "ripgrep"
        "rsync"
        "sbt-extras"
        "schemaspy"
        "shellcheck"
        "silver-searcher"
        "slack-term"
        "sqlint"
        "sqlite"
        "teensy-loader-cli"
        "tree"
        "unison"
        "unzip"
        "wget"
        "which"
    ];

    nixpkgs.ifDarwin.prebuilt = pickIfDarwin [
        "mongodb"
        "mongodb-tools"
        "postgresql_9_5"
        "vim"
    ];

    nixpkgs.ifLinux.prebuilt = pickIfLinux [
        "binutils"
        "chromium"
        "dfu-programmer"
        "dfu-util"
        "emacs"
        "escrotum"
        "feh"
        "firefoxWrapper"
        "freemind"
        "fswatch"
        "inkscape"
        "irccloud"
        "libreoffice"
        "pciutils"
        "playerctl"
        "postgresql"
        "powertop"
        "python3"
        "rofi-unwrapped"
        "rxvt_unicode-with-plugins"
        "slack"
        "stack"
        "usbutils"
        "vimHugeX"
        "whipper"
        "xclip"
        "zathura"
    ];

    nixpkgs.common.build.topLevel = pick [
        "global"
    ];

    nixpkgs.common.build.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc883" "djinn")
        // (np.hs.fromPackages "unstable" "ghc883" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc883" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc883" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc883" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc883" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc883" "hp2pretty")
        // (np.hs.fromPackages "unstable" "ghc883" "threadscope")

        // (np.hs.fromPackages "unstable" "ghc865" "pointfree")  # broken for 8.8.3 & 8.10.1, 20-7-22

        #// (np.hs.fromPackages "unstable" "ghc883" "pointful")            # marked broken, 20-7-22
        #// (np.hs.fromPackages "unstable" "ghc883" "ghc-events-analyze")  # marked broken, 20-7-22
        ;

    nixpkgs.ifLinux.build.topLevel =
        let pkgs = with np.nixpkgs-unstable; {
                texlive = texlive.combine {
                    inherit (texlive) scheme-medium;
                };
            };
        in if isDarwin then {} else pkgs;

    haskell-nix.prebuilt = with hn.haskell-nix; {
        inherit nix-tools;
    };

    haskell-nix.build = {}
        // (hn.fromHackage "ghc8101" "apply-refact")
        // (hn.fromHackage "ghc8101" "ghcid")
        // (hn.fromHackage "ghc8101" "hlint")
        // (hn.fromHackage "ghc8101" "stylish-haskell")
        // (hn.fromSource  "ghc8101" "codex")
        ;

in

{
    prebuilt.nixpkgs = {}
        // nixpkgs.common.prebuilt
        // nixpkgs.ifDarwin.prebuilt
        // nixpkgs.ifLinux.prebuilt;

    build.nixpkgs = {}
        // nixpkgs.common.build.topLevel
        // nixpkgs.common.build.haskell
        // nixpkgs.ifLinux.build.topLevel;

    prebuilt.haskell-nix = haskell-nix.prebuilt;

    build.haskell-nix = haskell-nix.build;
}
