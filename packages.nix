{ np
, hn
, sources
, isDarwin
}:

let

    pick = np.pick (if isDarwin then "stable" else "unstable");
    pickIfDarwin = if isDarwin then (np.pick "stable") else (attrNames: {});
    pickUnstableIfLinux = if isDarwin then (attrNames: {}) else (np.pick "unstable");
    pickStableIfLinux = if isDarwin then (attrNames: {}) else (np.pick "stable");

    nixpkgs.common.prebuilt = pick [
        "aspell"
        "aspellDicts.en"
        "aspellDicts.en-computers"
        "aspellDicts.en-science"
        "autojump"
        "bzip2"
        "cabal2nix"
        "cabal-install"
        "cachix"
        "cloc"
        "cmake"
        "coreutils"
        "dhall"
        "direnv"
        "fd"
        "gitFull"
        "gnugrep"
        "gnumake"
        "gnupg"
        "graphviz"
        "haskell.compiler.ghc8102"
        "htop"
        "imagemagick"
        "jq"
        "jre"
        "mpc_cli"
        "ncmpcpp"
        "niv"
        "nixfmt"
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

    nixpkgs.ifLinux.prebuilt = pickUnstableIfLinux [
        "alacritty"
        "ansifilter"
        "chromium"
        "dfu-programmer"
        "dfu-util"
        "discord"
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
        "wirelesstools"
        "xclip"
        "zathura"
    ];

    nixpkgs.common.build.topLevel = pick [ "global" ] // { emacs =
        let nixpkgs = if isDarwin then np.nixpkgs-stable else np.nixpkgs-unstable;
            raw =
                if isDarwin
                then nixpkgs.emacsMacport
                else nixpkgs.emacs;  # eventually: nixpkgs.emacsGcc;
        in (nixpkgs.emacsPackagesFor raw).emacsWithPackages (epkgs: with epkgs.melpaPackages; [
            vterm emacsql emacsql-sqlite
        ]);
    };

    nixpkgs.common.build.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc884" "djinn")
        // (np.hs.fromPackages "unstable" "ghc884" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc884" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc884" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc884" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc884" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc884" "hp2pretty")

        # DESIGN: marked broken, 2020-11-28
        #// (np.hs.fromPackages "unstable" "ghc884" "threadscope")
        ;

    nixpkgs.ifLinux.build.topLevel =
        let pkgs = with np.nixpkgs-unstable; {
                texlive = texlive.combine {
                    inherit (texlive) capt-of scheme-medium wrapfig;
                };
            };
        in if isDarwin then {} else pkgs;

    haskell-nix.prebuilt = with hn.haskell-nix; {
        inherit nix-tools;
    };

    haskell-nix.build = {}
        // (hn.fromHackage "ghc8102" "apply-refact")
        // (hn.fromHackage "ghc8102" "ghcid")
        // (hn.fromHackage "ghc8102" "hlint")
        // (hn.fromHackage "ghc8102" "stylish-haskell")

	# DESIGN: marked broken in Nixpkgs, doesn't seem to build with
	# Haskell.nix either
        #// (hn.fromHackage "ghc884" "ghc-events-analyze")
        ;

    shajra.build =
        let hls = ghcVersion:
                import sources.nix-haskell-hls {
                    inherit ghcVersion;
                    hlsUnstable = false;
                };
            lorelei = import sources.direnv-nix-lorelei;
            tags = import sources.nix-haskell-tags;
        in {
            implicit-hie = (hls "ghc8102").implicit-hie;
            haskell-hls-wrapper = (hls "ghc8102").hls-wrapper;
            haskell-hls-ghc8102 = (hls "ghc8102").hls-renamed;
            haskell-hls-ghc884  = (hls "ghc884").hls-renamed;
            haskell-hls-ghc865  = (hls "ghc865").hls-renamed;
            haskell-hls-tags    = tags.nix-haskell-tags-exe;
            direnv-nix-lorelei  = lorelei.direnv-nix-lorelei;
        };

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

    build.shajra = shajra.build;
}
