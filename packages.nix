{ lib
, np
, hn
, inputs'
, isDarwin
, isDevBuild
}:

let

    when = lib.optionalAttrs;

    pickHome = np.pick {
        linux  = "home";
        darwin = "home";
    };

    pickUnstable = np.pick {
        linux  = "unstable";
        darwin = "unstable";
    };

in {

    nixpkgs.prebuilt.audio.tui.all = pickHome [
        "mpc_cli"
    ];

    nixpkgs.prebuilt.audio.tui.linux = np.pick { linux = "home"; } [
        "playerctl"
        "ponymix"
        "pulsemixer"
        "whipper"
    ];

    nixpkgs.prebuilt.base.gui.all = pickHome [
        # Fonts
        "emacs-all-the-icons-fonts"  # for Emacs, used automatically by Doom
        "etBook"                     # stylish font from Edward Tufte's books
        "fira"                       # variable font to complement Fira Code
        "font-awesome_5"             # for i3status-rust icons
        "freefont_ttf"               # a Unicode fallback font
        "hasklig"                    # font with Haskell ligatures
        "inconsolata-nerdfont"       # popular font, might like it
        "nerdfonts"                  # developer fonts with lots of icons
        "noto-fonts-emoji"           # popular font, might like it
        "noto-fonts-extra"           # popular font, might like it
        "noto-fonts"                 # popular font, might like it
        "source-serif"               # serif font to complement Sauce Code Pro
        "symbola"                    # another Unicode fallback font
    ];

    nixpkgs.prebuilt.base.gui.darwin = np.pick { darwin = "home"; } [
    ];

    nixpkgs.prebuilt.base.gui.linux = np.pick { linux = "home"; } [
        "devour"
        "dunst"
        "fontpreview"
        "gnome.adwaita-icon-theme"
        "maim"
        "microsoft-edge"
        "pavucontrol"
        "simple-scan"
        "sxiv"
        "xclip"
        "xorg.xdpyinfo"
        "xorg.xev"
        "zoom-us"

        # Fonts
        # DESIGN: 2021-09-21: made Linux-only because of a build problem
        "twitter-color-emoji"        # for emojis
    ];

    nixpkgs.prebuilt.base.tui.all = pickHome [
        "ansifilter"
        "bzip2"
        "cachix"
        "coreutils"
        "curl"
        "direnv"
        "exa"
        "fd"
        "file"
        "gnugrep"
        "gnupg"  # TODO: home-manager
        "languagetool"
        "macchina"
        "nix-diff"
        "nixfmt"
        "paperkey"
        "patchelf"
        "procps"
        "pstree"
        "ripgrep"
        "rsync"
        "scc"
        "tree"
        "unzip"
        "wget"
        "which"
        "yq-go"
    ] // {
        aspell = np.nixpkgs.home.aspellWithDicts (d: with d; [
            en en-computers en-science
        ]);
    };

    nixpkgs.prebuilt.base.tui.linux = np.pick { linux = "home"; } [
        "entr"
        "fswatch"
        "niv"
        "pciutils"
        "powertop"
        "usbutils"
    ];

    nixpkgs.prebuilt.base.tui.darwin = np.pick { darwin = "home"; } [
    ];

    nixpkgs.prebuilt.chat.gui.all = pickHome [
    ];

    nixpkgs.prebuilt.chat.gui.linux = np.pick { linux = "home"; } [
        "discord"
        "element-desktop"
        "irccloud"
        "signal-desktop"
        "slack"
    ];

    nixpkgs.prebuilt.chat.tui.all = pickHome [
        "slack-term"
    ];

    nixpkgs.prebuilt.documentation.all = pickHome [
        "graphviz"
        "imagemagick"
        "nodePackages.textlint"
        "pandoc"
        "proselint"
        "python310Packages.grip"
        "t-rec"
    ];

    nixpkgs.prebuilt.documentation.linux = np.pick { linux = "home"; } [
        "dia"
        "freemind"
        "gimp"
        "inkscape"
        "peek"

        "libreoffice"  # DESIGN: 2022-04-15: broke for Darwin
    ];

    nixpkgs.prebuilt.programming.c.all = pickHome [
        "cmake"
    ];

    nixpkgs.prebuilt.programming.c.linux = np.pick { linux = "home"; } [
        "gcc"
    ];

    nixpkgs.prebuilt.programming.db = pickUnstable [
        "postgresql"
        "schemaspy"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general = pickHome [
        "gnumake"
    ];

    nixpkgs.prebuilt.programming.haskell =
        let
            home = pickHome [
                "cabal2nix"
                "cabal-install"
                "stack"
            ];
            unstable = pickUnstable [
                "haskell.compiler.ghc945"
                "haskellPackages.djinn"
            ];
        in home // unstable;

    nixpkgs.prebuilt.programming.java = pickUnstable [
    ];

    nixpkgs.prebuilt.programming.python = pickHome [
        "python3"
    ];

    nixpkgs.prebuilt.programming.scala = pickHome [
        "sbt-extras"
    ];

    nixpkgs.prebuilt.programming.shell = pickHome [
        "shellcheck"
    ];

    nixpkgs.prebuilt.sync = pickHome [
        "unison"
    ];

    nixpkgs.prebuilt.peripheral.wifi.tui.linux = np.pick { linux = "home"; } [
        "wirelesstools"
    ];

    nixpkgs.prebuilt.peripheral.wifi.gui.linux = np.pick { linux = "home"; } [
        "wpa_supplicant_gui"
    ];

    nixpkgs.build.base.gui.all = pickHome [
        "notify-time"
    ];

    nixpkgs.build.base.gui.darwin = np.pick { darwin = "home"; } [
        # DESIGN: yabai broken for M1
        # https://github.com/koekeishiya/yabai/issues/1054
        #"yabai"

        # DESIGN: Brew skhd seemed more stable for now
        #"skhd"
    ];

    nixpkgs.build.base.gui.linux = np.pick { linux = "home"; } [
        "dunst-osd"
        "i3-dpi"
        "i3status-rust-dunst"
        "i3-workspace-name"
    ];

    nixpkgs.build.base.tui.all = pickHome [
        "home-manager"
    ];

    nixpkgs.build.base.tui.darwin = np.pick { darwin = "home"; } [
    ];

    nixpkgs.build.base.tui.linux = np.pick { linux = "home"; } [
    ];

    nixpkgs.build.finance = pickHome [
        "moneydance"
    ];

    nixpkgs.build.os.darwin = np.pick { darwin = "home"; } [
    ];

    nixpkgs.build.os.nixos = np.pick { linux = "home"; } [
    ];

    nixpkgs.build.programming.general = pickHome [
        "global"
    ];

    nixpkgs.build.peripheral.wifi.tui.linux = np.pick { linux = "home"; } [
        "lan-cake"
    ];

    nixpkgs.build.programming.haskell = {}
        // (np.hs.fromPackages "unstable" "ghc945" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc945" "hoogle")
        # DESIGN: 2023-07-02: didn't build with 9.4.5; using Haskell.nix
        #// (np.hs.fromPackages "unstable" "ghc945" "haskdogs")
        #// (np.hs.fromPackages "unstable" "ghc945" "hasktags")
        #// (np.hs.fromPackages "unstable" "ghc945" "hp2pretty")

        # DESIGN: 2023-02-27: marked broken
        #// (np.hs.fromPackages "unstable" "ghc945" "threadscope")
        ;

    nixpkgs.build.unused.darwin = np.pick { darwin = "home"; } [
        # DESIGN: emacsMacport broken for M1
        # https://github.com/NixOS/nixpkgs/issues/127902
        # DESIGN: note emacsMacport doesn't have native compilation
        # https://github.com/railwaycat/homebrew-emacsmacport/issues/274
        #"emacsMacport"
    ];

    nixpkgs.build.unused.linux = np.pick { linux = "home"; } [
        # DESIGN: emacsNativeComp was renamed to emacsUnstable
        "emacsUnstable"
    ];

    haskell-nix.prebuilt.programming.haskell = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc945;
    };

    haskell-nix.build.programming.haskell = when (! isDevBuild) (
        {}
        // (hn.fromHackage "ghc945" "fast-tags")
        // (hn.fromHackage "ghc945" "ghcid")
        // (hn.fromHackage "ghc945" "apply-refact")
        // (hn.fromHackage "ghc945" "hlint")
        // (hn.fromHackage "ghc945" "haskdogs")
        // (hn.fromHackage "ghc945" "hasktags")
        // (hn.fromHackage "ghc945" "hp2pretty")
        // (hn.fromHackageCustomized "ghc945" "stylish-haskell" { configureArgs = "-f ghc-lib"; })

        # DESIGN: marked broken in Nixpkgs, doesn't seem to build with
        # Haskell.nix either
        #// (hn.fromHackage "ghc945" "ghc-events-analyze")
    );

    shajra.prebuilt = {};

    shajra.build.programming.haskell =
        let hls = ghcVersion:
                import inputs'.haskell-hls-nix {
                    inherit ghcVersion;
                    hlsUnstable = false;
                };
            tags = import inputs'.haskell-tags-nix;
        in when false {
        #in when (! isDevBuild) {
            implicit-hie        = (hls "8.10.7").implicit-hie;
            haskell-hls-wrapper = (hls "8.10.7").hls-wrapper;
            haskell-hls-ghc8107 = (hls "8.10.7").hls-renamed;
            haskell-hls-ghc8106 = (hls "8.10.6").hls-renamed;
            haskell-hls-ghc884  = (hls "8.8.4").hls-renamed;
            haskell-hls-ghc865  = (hls "8.6.5").hls-renamed;
            haskell-hls-tags    = tags.haskell-tags-nix-exe;
        };

    shajra.build.audio.gui.linux = when (! isDarwin) {
        inherit (inputs'.bluos-nix.packages) bluos-controller;
    };

}
