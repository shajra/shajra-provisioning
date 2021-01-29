{ np
, hn
, sources
, isDarwin
}:

let

    pickDefault = np.pick {
        linux  = "unstable";
        darwin = "stable";
    };

    nixpkgs.common.prebuilt = pickDefault [
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

    nixpkgs.unstable.prebuilt = np.pick {
        linux  = "unstable";
        darwin = "unstable";
    } [
        "haskell.compiler.ghc8103"
    ];

    nixpkgs.ifDarwin.prebuilt = np.pick {
        darwin = "stable";
    } [
        "mongodb"
        "mongodb-tools"
        "postgresql_9_5"
        "vim"
    ];

    nixpkgs.ifLinux.prebuilt = np.pick {
        linux = "unstable";
    } [
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

    nixpkgs.common.build.topLevel = pickDefault [ "global" ] // {
        emacs =
            let nixpkgs =
                    if isDarwin
                    then np.nixpkgs-stable
                    else np.nixpkgs-unstable;
                raw =
                    if isDarwin
                    then nixpkgs.emacsMacport
                    else nixpkgs.emacs;  # eventually: nixpkgs.emacsGcc;
            in (nixpkgs.emacsPackagesFor raw).emacsWithPackages
                (epkgs: with epkgs.melpaPackages; [
                    vterm emacsql emacsql-sqlite
                ]);
    };

    nixpkgs.common.build.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc8103" "djinn")
        // (np.hs.fromPackages "unstable" "ghc8103" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc8103" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc8103" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc8103" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc8103" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc8103" "hp2pretty")

        # DESIGN: marked broken, 2020-11-28
        #// (np.hs.fromPackages "unstable" "ghc8103" "threadscope")
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
        // (hn.fromHackage "ghc8103" "apply-refact")
        // (hn.fromHackage "ghc8103" "ghcid")
        // (hn.fromHackage "ghc8103" "hlint")
        // (hn.fromHackage "ghc8103" "stylish-haskell")

	# DESIGN: marked broken in Nixpkgs, doesn't seem to build with
	# Haskell.nix either
        #// (hn.fromHackage "ghc8103" "ghc-events-analyze")
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
            implicit-hie = (hls "ghc8103").implicit-hie;
            haskell-hls-wrapper = (hls "ghc8103").hls-wrapper;
            haskell-hls-ghc8103 = (hls "ghc8103").hls-renamed;
            haskell-hls-ghc884  = (hls "ghc884").hls-renamed;
            haskell-hls-ghc865  = (hls "ghc865").hls-renamed;
            haskell-hls-tags    = tags.nix-haskell-tags-exe;
            direnv-nix-lorelei  = lorelei.direnv-nix-lorelei;
        };

in

{
    prebuilt.nixpkgs = {}
        // nixpkgs.common.prebuilt
        // nixpkgs.unstable.prebuilt
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
