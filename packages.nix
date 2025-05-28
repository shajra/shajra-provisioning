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
        "nix-update"
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
        # REVISIT: 2025-04-12: Unsupported for Darwin
        "irccloud"

        # REVISIT: 2025-03-23: Prebuilt for Linux, but not for Darwin
        "caprine"
        "element-desktop"
        "signal-desktop-bin"
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

    nixpkgs.prebuilt.programming.containers = pickHome [
        "kubectl"
    ];

    nixpkgs.prebuilt.programming.db = pickHome [
        "pgformatter"
        "postgresql"
        "schemaspy"
        "sqitchPg"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general =
        let all = pickHome [
                "global"
                "gnumake"
                "nil"
                "nixd"
                "nodejs"  # DESIGN: needed for Cursor remote SSH extension
                "tcount"
                "tokei"
                "wireshark"
            ];
            darwin = np.pick { darwin = "home"; } [
                "aider-chat-full"
            ];
        in all // darwin;

    nixpkgs.prebuilt.programming.haskell = pickHome [
        "cabal2nix"
        "cabal-install"
        "stack"
        "haskell.compiler.ghc984"
        "haskell.packages.ghc984.apply-refact"
        "haskell.packages.ghc984.cabal-fmt"
        "haskell.packages.ghc984.djinn"
        "haskell.packages.ghc984.eventlog2html"
        "haskell.packages.ghc984.fast-tags"
        "haskell.packages.ghc984.ghc-events"
        "haskell.packages.ghc984.ghcid"
        "haskell.packages.ghc984.haskdogs"
        "haskell.packages.ghc984.hasktags"
        "haskell.packages.ghc984.hlint"
        "haskell.packages.ghc984.hoogle"
        "haskell.packages.ghc984.hp2pretty"
        "haskell.packages.ghc984.profiterole"
        "haskell.packages.ghc984.profiteur"
        "haskell.packages.ghc984.stylish-haskell"
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
        "iterm2"
        "raycast"
        "shortcat"
    ];

    nixpkgs.build.base.gui.linux = np.pick { linux = "home"; } [
        "dunst-osd"
        "i3-dpi"
        "i3status-rust-dunst"
        "i3-workspace-name"
        "vivaldi"
    ];

    nixpkgs.build.base.tui.all = pickHome [
        "home-manager-latest"
        "preview-file"
    ] // {
        aspell = np.nixpkgs.home.aspellWithDicts (d: with d; [
            en en-computers en-science
        ]);
    };

    nixpkgs.build.chat.gui.all = pickHome [
        "discord"
        "zoom-us"
    ];

    nixpkgs.build.chat.gui.darwin = np.pick { darwin = "home"; } [
        "caprine"
        "element-desktop"
        "signal-desktop-bin"
    ];

    nixpkgs.build.chat.gui.linux = np.pick { linux = "home"; } [
        "slack"
    ];

    nixpkgs.build.finance = pickHome [
        "moneydance"
    ];

    nixpkgs.build.peripheral.wifi.tui.linux = np.pick { linux = "home"; } [
        "lan-cake"
    ];

    nixpkgs.build.programming.containers = pickHome [
    ] // {
        google-cloud-sdk = with np.nixpkgs.home;
            google-cloud-sdk.withExtraComponents [
                google-cloud-sdk.components.gke-gcloud-auth-plugin
            ];
    };

    nixpkgs.build.programming.general =
        let all = pickHome [
                "code-cursor"
            ];
            linux = np.pick { linux = "home"; } [
                "aider-chat-full"
            ];
        in all // linux;

    nixpkgs.build.uncategorized.darwin = np.pick { darwin = "home"; } [
        "sketchybar-helpers"
        "sketchybar-lua"
    ];

    nixpkgs.build.uncategorized.linux = np.pick { linux = "home"; } [
    ];

    haskell-nix.prebuilt.programming.haskell = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc984;
    };

    haskell-nix.build.programming.haskell = when (! isDevBuild) {}
        # DESIGN: Nixpkgs-built binaries above are fine (maybe bloated)
        #({}
        #// (hn.fromHackage "ghc984" "apply-refact")
        #// (hn.fromHackage "ghc984" "fast-tags")
        #// (hn.fromHackage "ghc984" "ghc-events")
        #// (hn.fromHackage "ghc984" "ghcid")
        #// (hn.fromHackage "ghc984" "haskdogs")
        #// (hn.fromHackage "ghc984" "hasktags")
        #// (hn.fromHackage "ghc984" "hlint")
        #// (hn.fromHackage "ghc984" "hoogle")
        #// (hn.fromHackage "ghc984" "hp2pretty")
        #// (hn.fromHackage "ghc984" "threadscope")
        #// (hn.fromHackageCustomized "ghc984" "stylish-haskell" { configureArgs = "-f ghc-lib"; })
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
