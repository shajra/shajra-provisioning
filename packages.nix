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
        "font-awesome_6"             # for i3status-rust icons
        "freefont_ttf"               # a Unicode fallback font
        "hasklig"                    # font with Haskell ligatures
        "inconsolata-nerdfont"       # popular font, might like it
        "nerdfonts"                  # developer fonts with lots of icons
        "noto-fonts-emoji"           # popular font, might like it
        "noto-fonts-extra"           # popular font, might like it
        "noto-fonts"                 # popular font, might like it
        "sf-symbols"                 # font with application icons
        "sketchybar-font"            # font with application icons
        "source-serif"               # serif font to complement Sauce Code Pro
        "symbola"                    # another Unicode fallback font
    ];

    nixpkgs.prebuilt.base.gui.darwin = np.pick { darwin = "home"; } [
    ] // np.pick { darwin = "unstable"; } [
        # DESIGN: these are all Mac-only applications; unstable is fine
        "aldente"
        "goku"
        "iterm2"
        "raycast"
        "shortcat"
    ];

    nixpkgs.prebuilt.base.gui.linux = np.pick { linux = "home"; } [
        "devour"
        "dunst"
        "fontpreview"
        "gnome.adwaita-icon-theme"
        "maim"
        "microsoft-edge-beta"
        "pavucontrol"
        "simple-scan"
        "sxiv"
        "tor-browser"
        "xclip"
        "xdotool"
        "xorg.xdpyinfo"
        "xorg.xev"
    ];

    nixpkgs.prebuilt.base.tui.all = pickHome [
        "ansifilter"
        "bzip2"
        "cachix"
        "coreutils"
        "curl"
        "direnv"
        "eza"
        "fd"
        "file"
        "gnugrep"
        "gnupg"  # TODO: home-manager
        "languagetool"
        "macchina"
        "nix-diff"
        "nix-du"
        "nix-health"
        "nix-index"
        "nix-info"
        "nix-melt"
        "nix-output-monitor"
        "nix-search-cli"
        "nix-template"
        "nix-top"
        "nix-tree"
        "nixfmt"
        "paperkey"
        "patchelf"
        "preview-file"
        "procps"
        "pstree"
        "rsync"
        "scc"
        "tree"
        "unzip"
        "w3m"
        "wemux"
        "wget"
        "which"
        "yq-go"
    ] // {
        aspell = np.nixpkgs.home.aspellWithDicts (d: with d; [
            en en-computers en-science
        ]);
    };

    nixpkgs.prebuilt.base.tui.darwin = np.pick { darwin = "home"; } [
    ] // np.pick { darwin = "unstable"; } [
        # DESIGN: these are all Mac-only applications; unstable is fine
        "asitop"
        "mas"
    ];

    nixpkgs.prebuilt.base.tui.linux = np.pick { linux = "home"; } [
        "entr"
        "fswatch"
        "niv"
        "pciutils"
        "powertop"
        "usbutils"
    ];

    nixpkgs.prebuilt.centralized = np.pick { linux = "home"; } [
        "buku"
        "bukubrow"
    ];

    nixpkgs.prebuilt.chat.gui.all = pickHome [
        "discord"
        "slack"
        "zoom-us"
    ];

    nixpkgs.prebuilt.chat.gui.linux = np.pick { linux = "home"; } [
        "element-desktop"
        "irccloud"
        "signal-desktop"
    ];

    nixpkgs.prebuilt.chat.tui.all = pickHome [
        "slack-term"
    ];

    nixpkgs.prebuilt.documentation.all = pickHome [
        "graphviz"
        "imagemagick"
        "nodePackages.textlint"
        "proselint"
        "python3Packages.grip"
        "t-rec"
    ];

    nixpkgs.prebuilt.documentation.linux = np.pick { linux = "home"; } [
        "dia"
        "freemind"
        "gimp"
        "inkscape"
        "libreoffice"  # DESIGN: Sometimes has broken build on Darwin
        "peek"
    ];

    nixpkgs.prebuilt.programming.c.all = pickHome [
        "cmake"
    ];

    nixpkgs.prebuilt.programming.c.linux = np.pick { linux = "home"; } [
        "gcc"
    ];

    nixpkgs.prebuilt.programming.db = pickHome [
        "postgresql"
        "pgformatter"
        "schemaspy"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general = pickHome [
        "gnumake"
        "nil"
        "wireshark"
    ];

    nixpkgs.prebuilt.programming.haskell = pickHome [
        "cabal2nix"
        "cabal-install"
        "stack"
        "haskell.compiler.ghc965"
    ];

    nixpkgs.prebuilt.programming.java = pickHome [
    ];

    nixpkgs.prebuilt.programming.lua = pickHome [
        "luaformatter"
        "lua5_4"
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
        # REVISIT: Waiting for Nixpkgs to have Mac SDK 14
        #"yabai"
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

    nixpkgs.build.programming.haskell = pickUnstable [
        # DESIGN: coming from Nixpkgs, but not cached
        "haskell.packages.ghc965.apply-refact"
        "haskell.packages.ghc965.djinn"
        "haskell.packages.ghc965.fast-tags"
        "haskell.packages.ghc965.ghc-events"
        "haskell.packages.ghc965.ghcid"
        "haskell.packages.ghc965.haskdogs"
        "haskell.packages.ghc965.hasktags"
        "haskell.packages.ghc965.hoogle"
        "haskell.packages.ghc965.hlint"
        "haskell.packages.ghc965.hp2pretty"
        "haskell.packages.ghc965.stylish-haskell"
    ];

    nixpkgs.build.uncategorized.darwin = np.pick { darwin = "home"; } [
        # REVISIT: emacsMacport broken for M1
        # https://github.com/NixOS/nixpkgs/issues/127902
        # REVISIT: note emacsMacport doesn't have native compilation
        # https://github.com/railwaycat/homebrew-emacsmacport/issues/274
        #"emacsMacport"
        "sketchybar-cpu"
        "sketchybar-lua"
    ];

    nixpkgs.build.uncategorized.linux = np.pick { linux = "home"; } [
        # REVISIT: https://github.com/doomemacs/doomemacs#prerequisites
        # using emacs29 over emacs-unstable to hit Nixpkgs cache
        "emacs29"
    ];

    haskell-nix.prebuilt.programming.haskell = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc965;
    };

    haskell-nix.build.programming.haskell = when (! isDevBuild) (
        {}
        # DESIGN: Nixpkgs-built binaries above are fine (maybe bloated)
        #// (hn.fromHackage "ghc965" "apply-refact")
        #// (hn.fromHackage "ghc965" "fast-tags")
        #// (hn.fromHackage "ghc965" "ghc-events")
        #// (hn.fromHackage "ghc965" "ghcid")
        #// (hn.fromHackage "ghc965" "haskdogs")
        #// (hn.fromHackage "ghc965" "hasktags")
        #// (hn.fromHackage "ghc965" "hlint")
        #// (hn.fromHackage "ghc965" "hoogle")
        #// (hn.fromHackage "ghc965" "hp2pretty")
        #// (hn.fromHackage "ghc965" "threadscope")
        #// (hn.fromHackageCustomized "ghc965" "stylish-haskell" { configureArgs = "-f ghc-lib"; })

        # REVISIT: marked broken in Nixpkgs, doesn't seem to build with
        # Haskell.nix either (need to look for a modern alternative exists)
        #// (hn.fromHackage "ghc965" "ghc-events-analyze")
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

    shajra.build.audio.gui.all = {
        inherit (inputs'.bluos-nix.packages) bluos-controller;
    };

}
