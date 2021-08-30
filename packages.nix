{ np
, hn
, sources
, isDarwin
, isDevBuild
}:

let

    when = np.nixpkgs-stable.lib.optionalAttrs;

    pickHome = np.pick {
        linux  = "unstable";
        darwin = "stable";
    };

    nixpkgs.prebuilt.common.home = pickHome [

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
        "nnn"
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
        "nerdfonts"                  # developer fonts with lots of icons
        "source-serif-pro"           # serif font to complement Sauce Code Pro
        "symbola"                    # another Unicode fallback font
        "twitter-color-emoji"        # for emojis
        # DESIGN: Hasklig is also built from source below
    ];

    nixpkgs.prebuilt.common.unstable = np.pick {
        linux  = "unstable";
        darwin = "unstable";
    } [
        "haskell.compiler.ghc8104"
    ];

    nixpkgs.prebuilt.ifDarwin.stable = np.pick {
        darwin = "stable";
    } [
        # TODO: figure out collision of JDK with Home Manager on Darwin
        # https://github.com/nix-community/home-manager/issues/1994
        "jdk8"
        "mongodb"
        "mongodb-tools"
    ];

    nixpkgs.prebuilt.ifLinux.unstable = np.pick {
        linux = "unstable";
    } [
        "ansifilter"
        "chromium"  # TODO: home-manager
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

    nixpkgs.build.common.home = pickHome [
        "emacsGcc"  # DESIGN: prebuilt/cached for Linux, but not Darwin
        "global"
        "hasklig"
        "shajra-home-manager"
    ];

    nixpkgs.build.ifLinux.unstable = when (! isDarwin) {
        inherit (np.nixpkgs-unstable)
        dunst-osd
        dunst-time
        i3-dpi
        i3-workspace-name
        i3status-rust-dunst
        lan-jelly
        moneydance
        shajra-nixos-rebuild;
    };

    nixpkgs.build.ifDarwin.stable = when isDarwin {
        inherit (np.nixpkgs-stable)
        shajra-darwin-rebuild
        skhd
        yabai;
    };

    nixpkgs.build.common.haskell = {}
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

    haskell-nix.prebuilt = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc8105;
    };

    haskell-nix.build = when (! isDevBuild) (
        {}
        // (hn.fromHackage "ghc8105" "apply-refact")
        // (hn.fromHackage "ghc8105" "ghcid")
        // (hn.fromHackage "ghc8105" "hlint")
        // (hn.fromHackage "ghc8105" "stylish-haskell")

        # DESIGN: marked broken in Nixpkgs, doesn't seem to build with
        # Haskell.nix either
        #// (hn.fromHackage "ghc8103" "ghc-events-analyze")
    );

    shajra.build.common =
        let hls = ghcVersion:
                import sources.nix-haskell-hls {
                    inherit ghcVersion;
                    hlsUnstable = false;
                };
            tags = import sources.nix-haskell-tags;
        in when (! isDevBuild) {
            implicit-hie        = (hls "8.10.5").implicit-hie;
            haskell-hls-wrapper = (hls "8.10.5").hls-wrapper;
            haskell-hls-ghc8105 = (hls "8.10.5").hls-renamed;
            haskell-hls-ghc8104 = (hls "8.10.4").hls-renamed;
            haskell-hls-ghc884  = (hls "8.8.4").hls-renamed;
            haskell-hls-ghc865  = (hls "8.6.5").hls-renamed;
            haskell-hls-tags    = tags.nix-haskell-tags-exe;
        };

    shajra.build.ifLinux = when (! isDarwin) (import sources.bluos-nix);

in

{
    nixpkgs.prebuilt = {}
        // nixpkgs.prebuilt.common.home
        // nixpkgs.prebuilt.common.unstable
        // nixpkgs.prebuilt.ifDarwin.stable
        // nixpkgs.prebuilt.ifLinux.unstable;

    nixpkgs.build = {}
        // nixpkgs.build.common.home
        // nixpkgs.build.ifLinux.unstable
        // nixpkgs.build.ifDarwin.stable
        // nixpkgs.build.common.haskell;

    inherit haskell-nix;

    shajra.build = {}
        // shajra.build.common
        // shajra.build.ifLinux;
}
