{ np
, hn
, sources
, isDarwin
}:

let

    pickHome = np.pick {
        linux  = "unstable";
        darwin = "stable";
    };

    nixpkgs.common.prebuilt = pickHome [

        # Generally used CLI tools
        "aspell"
        "aspellDicts.en"
        "aspellDicts.en-computers"
        "aspellDicts.en-science"
        "bzip2"
        "cabal2nix"
        "cabal-install"
        "cachix"
        "cmake"
        "coreutils"
        "curl"
        "dhall"
        "exa"
        "fd"
        "gnugrep"
        "gnumake"
        "gnupg"  # TODO: home-manager
        "graphviz"
        "imagemagick"
        "mpc_cli"
        "niv"
        "nix-diff"
        "nixfmt"
        "nix-index"
        "nodePackages.textlint"
        "pandoc"
        "paperkey"
        "patchelf"
        "procps"
        "proselint"
        "pstree"
        "python3"
        "python38Packages.grip"
        "ripgrep"
        "rsync"
        "sbt-extras"
        "scc"
        "schemaspy"
        "shellcheck"
        "slack-term"
        "sqlint"
        "sqlite"
        "tree"
        "unison"
        "unzip"
        "wget"
        "which"

        # Fonts
        "emacs-all-the-icons-fonts"  # for Emacs, used automatically by Doom
        "etBook"                     # stylish font from Edward Tufte's books
        "fira"                       # variable font to complement Fira Code
        "font-awesome_5"             # for i3status-rust icons
        "freefont_ttf"               # a Unicode fallback font
        "hasklig"                    # latest Haskell-ligatures
        "nerdfonts"                  # developer fonts with lots of icons
        "source-serif-pro"           # serif font to complement Sauce Code Pro
        "symbola"                    # another Unicode fallback font
        "twitter-color-emoji"        # for emojis
    ];

    nixpkgs.unstable.prebuilt = np.pick {
        linux  = "unstable";
        darwin = "unstable";
    } [
        "haskell.compiler.ghc8104"
    ];

    nixpkgs.ifDarwin.prebuilt = np.pick {
        darwin = "stable";
    } [
        # TODO: figure out collision of JDK with Home Manager on Darwin
        # https://github.com/nix-community/home-manager/issues/1994
        "jdk8"
        "mongodb"
        "mongodb-tools"
    ];

    nixpkgs.ifLinux.prebuilt.stable = np.pick {
        linux = "unstable";
    } [
        # DESIGN: broken in unstable, 2021-08-01
        "chromium"  # TODO: home-manager
    ];

    nixpkgs.ifLinux.prebuilt.unstable = np.pick {
        linux = "unstable";
    } [
        "ansifilter"
        "dfu-programmer"
        "dfu-util"
        "discord"
        "dunst"
        "fontpreview"
        "freemind"
        "fswatch"
        "gnome3.adwaita-icon-theme"
        "inkscape"
        "irccloud"
        "jdk"
        "libreoffice"
        "maim"
        "pavucontrol"
        "pciutils"
        "playerctl"
        "ponymix"
        "postgresql"
        "powertop"
        "pulsemixer"
        "simple-scan"
        "slack"
        "stack"
        "usbutils"
        "whipper"
        "wirelesstools"
        "wpa_supplicant_gui"
        "xclip"
        "xorg.xdpyinfo"
        "xorg.xev"
    ];

    nixpkgs.common.build.topLevel = pickHome [ "global" ] // {
        emacs =
            let nixpkgs =
                    if isDarwin
                    then np.nixpkgs-stable
                    else np.nixpkgs-unstable;
                raw =
                    if isDarwin
                    then
                        # DESIGN: not built/cached in Hydra, trying out
                        nixpkgs.emacsGcc
                    else nixpkgs.emacsGcc;
            in (nixpkgs.emacsPackagesFor raw).emacsWithPackages
                (epkgs: with epkgs.melpaPackages; [
                    vterm emacsql emacsql-sqlite
                ]);
    };

    nixpkgs.common.build.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc8104" "djinn")
        // (np.hs.fromPackages "unstable" "ghc8104" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc8104" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc8104" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc8104" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc8104" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc8104" "hp2pretty")

        # DESIGN: marked broken, 2020-11-28
        #// (np.hs.fromPackages "unstable" "ghc8103" "threadscope")
        ;

    nixpkgs.ifLinux.build.topLevel =
        let pkgs = with np.nixpkgs-unstable; {
                inherit
                    dunst-time
                    i3-init
                    moneydance;
                texlive = texlive.combine {
                    inherit (texlive) capt-of scheme-medium wrapfig;
                };
            };
        in if isDarwin then {} else pkgs;

    haskell-nix.prebuilt = with hn.nixpkgs.haskell-nix; {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = nix-tools.ghc8105;
    };

    haskell-nix.build = {}
        // (hn.fromHackage "ghc8105" "apply-refact")
        // (hn.fromHackage "ghc8105" "ghcid")
        // (hn.fromHackage "ghc8105" "hlint")
        // (hn.fromHackage "ghc8105" "stylish-haskell")

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
            tags = import sources.nix-haskell-tags;
        in {
            implicit-hie        = (hls "8.10.5").implicit-hie;
            haskell-hls-wrapper = (hls "8.10.5").hls-wrapper;
            haskell-hls-ghc8105 = (hls "8.10.5").hls-renamed;
            haskell-hls-ghc8104 = (hls "8.10.4").hls-renamed;
            haskell-hls-ghc884  = (hls "8.8.4").hls-renamed;
            haskell-hls-ghc865  = (hls "8.6.5").hls-renamed;
            haskell-hls-tags    = tags.nix-haskell-tags-exe;
        };

in

{
    inherit haskell-nix shajra;

    nixpkgs.prebuilt = {}
        // nixpkgs.common.prebuilt
        // nixpkgs.unstable.prebuilt
        // nixpkgs.ifDarwin.prebuilt
        // nixpkgs.ifLinux.prebuilt.stable
        // nixpkgs.ifLinux.prebuilt.unstable;

    nixpkgs.build = {}
        // nixpkgs.common.build.topLevel
        // nixpkgs.common.build.haskell
        // nixpkgs.ifLinux.build.topLevel;
}
