{ np
, hn
, isDarwin
}:

let

    pick = np.pick (if isDarwin then "stable" else "unstable");
    pickIfDarwin = if isDarwin then (np.pick "stable") else (attrNames: {});
    pickUnstableIfLinux = if isDarwin then (attrNames: {}) else (np.pick "unstable");
    pickStableIfLinux = if isDarwin then (attrNames: {}) else (np.pick "stable");

    nixpkgs.common.prebuilt = pick [
        "aspell"
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
        "ghc"
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
        "emacsMacport"
        "mongodb"
        "mongodb-tools"
        "postgresql_9_5"
        "vim"
    ];

    nixpkgs.ifLinux.prebuilt = pickUnstableIfLinux [
        "ansifilter"
        "chromium"
        "clang"
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
        "xclip"
        "zathura"
    ] // pickStableIfLinux [
        "emacs"
    ];

    nixpkgs.common.build.topLevel = pick [
        "global"
    ];

    nixpkgs.common.build.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc884" "djinn")
        // (np.hs.fromPackages "unstable" "ghc884" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc884" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc884" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc884" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc884" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc884" "hp2pretty")
        // (np.hs.fromPackages "unstable" "ghc884" "threadscope")
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
        // (hn.fromSource  "ghc8102" "codex")

	# DESIGN: marked broken in Nixpkgs, doesn't seem to build with
	# Haskell.nix either
        #// (hn.fromHackage "ghc884" "ghc-events-analyze")
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
