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
        "whipper"
    ];

    nixpkgs.prebuilt.audio.tui.linux = np.pick { linux = "home"; } [
        "playerctl"
        "ponymix"
        "pulsemixer"
    ];

    nixpkgs.prebuilt.base.gui.all = pickHome [
        # Cached Fonts
        "emacs-all-the-icons-fonts"  # for Emacs, used automatically by Doom
        "etBook"                     # stylish font from Edward Tufte's books
        "fira"                       # variable font to complement Fira Code
        "font-awesome_6"             # for i3status-rust icons
        "freefont_ttf"               # a Unicode fallback font
        "hasklig"                    # font with Haskell ligatures
        "noto-fonts-emoji"           # popular font, might like it
        "noto-fonts-extra"           # popular font, might like it
        "noto-fonts"                 # popular font, might like it
        "source-serif"               # serif font to complement Sauce Code Pro
    ]
    # developer fonts with lots of icons
    // lib.filterAttrs (_: v: lib.isDerivation v) np.nixpkgs.home.nerd-fonts;

    nixpkgs.prebuilt.base.gui.darwin = np.pick { darwin = "home"; } [
    ] // np.pick { darwin = "unstable"; } [
        # DESIGN: these are all Mac-only applications; unstable is fine
        "goku"
        "iterm2"
    ];

    nixpkgs.prebuilt.base.gui.linux = np.pick { linux = "home"; } [
        "adwaita-icon-theme"
        "devour"
        "dunst"
        "fontpreview"
        "maim"
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
        "nix-tree"
        "nixfmt-rfc-style"
        "nvd"
        "paperkey"
        "patchelf"
        "procps"
        "pstree"
        "rsync"
        "scc"
        "statix"
        "tree"
        "unzip"
        "w3m"
        "wemux"
        "wget"
        "which"
        "yq-go"
    ];

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
        "poppler_utils"
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
        "pgformatter"
        "postgresql"
        "schemaspy"
        "sqitchPg"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general = pickHome [
        "gnumake"
        "nil"
      ] // (np.pick { linux = "home"; darwin = "system"; } [
        "wireshark"  # REVISIT: 2024-12-10: Package broken on Darwin/unstable
      ]);

    nixpkgs.prebuilt.programming.haskell = pickHome [
        "cabal2nix"
        "cabal-install"
        "stack"

        # REVISIT: 2024-12-10: ghc984: many Haskell packages broken
        "haskell.compiler.ghc966"
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

        # Uncached Fonts
        "sf-symbols"                 # font with application icons
        "sketchybar-font"            # font with application icons
        "symbola"                    # another Unicode fallback font
    ];

    nixpkgs.build.base.gui.darwin = np.pick { darwin = "home"; } [
    ] // np.pick { darwin = "unstable"; } [
        # DESIGN: these are all Mac-only applications; unstable is fine
        "aldente"
        "raycast"
        "shortcat"
    ];

    nixpkgs.build.base.gui.linux = np.pick { linux = "home"; } [
        "code-cursor"  # REVISIT: 2024-12-10: code-cursor unsupported for Darwin in Nixpkgs
        "dunst-osd"
        "i3-dpi"
        "i3status-rust-dunst"
        "i3-workspace-name"
        "microsoft-edge"
    ];

    nixpkgs.build.base.tui.all = pickHome [
        "home-manager-latest"
        "preview-file"
    ] // {
        aspell = np.nixpkgs.home.aspellWithDicts (d: with d; [
            en en-computers en-science
        ]);
    };

    nixpkgs.build.base.tui.darwin = np.pick { darwin = "home"; } [
    ];

    nixpkgs.build.base.tui.linux = np.pick { linux = "home"; } [
    ];

    nixpkgs.build.chat.gui.all = pickHome [
        "discord"
        "slack"
        "zoom-us"
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
        "haskell.packages.ghc966.apply-refact"
        "haskell.packages.ghc966.cabal-fmt"
        "haskell.packages.ghc966.djinn"
        "haskell.packages.ghc966.eventlog2html"
        "haskell.packages.ghc966.fast-tags"
        "haskell.packages.ghc966.ghc-events"
        "haskell.packages.ghc966.ghcid"
        "haskell.packages.ghc966.haskdogs"
        "haskell.packages.ghc966.hasktags"
        "haskell.packages.ghc966.hlint"
        "haskell.packages.ghc966.hoogle"
        "haskell.packages.ghc966.hp2pretty"
        "haskell.packages.ghc966.profiterole"
        "haskell.packages.ghc966.profiteur"
        "haskell.packages.ghc966.stylish-haskell"
    ];

    nixpkgs.build.uncategorized.darwin = np.pick { darwin = "home"; } [
        "sketchybar-cpu"
        "sketchybar-lua"
    ];

    nixpkgs.build.uncategorized.linux = np.pick { linux = "home"; } [
    ];

    haskell-nix.prebuilt.programming.haskell = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc966;
    };

    haskell-nix.build.programming.haskell = when (! isDevBuild) {}
        # DESIGN: Nixpkgs-built binaries above are fine (maybe bloated)
        #({}
        #// (hn.fromHackage "ghc966" "apply-refact")
        #// (hn.fromHackage "ghc966" "fast-tags")
        #// (hn.fromHackage "ghc966" "ghc-events")
        #// (hn.fromHackage "ghc966" "ghcid")
        #// (hn.fromHackage "ghc966" "haskdogs")
        #// (hn.fromHackage "ghc966" "hasktags")
        #// (hn.fromHackage "ghc966" "hlint")
        #// (hn.fromHackage "ghc966" "hoogle")
        #// (hn.fromHackage "ghc966" "hp2pretty")
        #// (hn.fromHackage "ghc966" "threadscope")
        #// (hn.fromHackageCustomized "ghc966" "stylish-haskell" { configureArgs = "-f ghc-lib"; })
        #)
    ;

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
            inherit ((hls "8.10.7")) implicit-hie;
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
