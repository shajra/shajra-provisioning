{ lib
, np
, hn
, inputs'
, isDarwin
, isDevBuild
}:

let

    when = lib.optionalAttrs;

    pickAll = nixpkgsName: np.pick {
        darwin = nixpkgsName;
        linux  = nixpkgsName;
    };

in rec {

    shajra.prebuilt = {};

    shajra.build.audio.gui = {
        inherit (inputs'.bluos-nix.packages) bluos-controller;
    };

    nixpkgs.prebuilt.audio.tui =
        let all = pickAll "home" [
                "mpc_cli"
                "whipper"
            ];
            linux = np.pick { linux = "home"; } [
                "playerctl"
                "ponymix"
                "pulsemixer"
            ];
        in all // linux;

    nixpkgs.prebuilt.base.gui =
        let # DESIGN: these are all Mac-only applications; unstable is fine
            darwin = np.pick { darwin = "unstable"; } [
                "goku"
            ];
            linux = np.pick { linux = "home"; } [
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
        in darwin // linux // nixpkgs.prebuilt.fonts;

    nixpkgs.build.base.gui =
        let all = pickAll "home" [
                "notify-time"
            ];
            # DESIGN: these are all Mac-only applications; unstable is fine
            darwin = np.pick { darwin = "unstable"; } [
                "aldente"
                "iterm2"
                "raycast"
                "shortcat"
            ];
            linux = np.pick { linux = "home"; } [
                "dunst-osd"
                "i3-dpi"
                "i3status-rust-dunst"
                "i3-workspace-name"
                "vivaldi"
            ];
        in all // darwin // linux // nixpkgs.build.fonts;

    nixpkgs.prebuilt.base.tui =
        let all = pickAll "home" [
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
            # DESIGN: these are all Mac-only applications; unstable is fine
            darwin = np.pick { darwin = "unstable"; } [
                "asitop"
                "mas"
            ];
            linux = np.pick { linux = "home"; } [
                "entr"
                "fswatch"
                "niv"
                "pciutils"
                "powertop"
                "usbutils"
            ];
        in all // darwin // linux;

    nixpkgs.build.base.tui =
        let all.picked = pickAll "home" [
                "home-manager-latest"
                "preview-file"
            ];
            all.aspell = np.nixpkgs.home.aspellWithDicts (d: with d; [
                en en-computers en-science
            ]);
        in all.picked // { inherit (all) aspell; };

    nixpkgs.prebuilt.centralized = np.pick { linux = "home"; } [
        # REVISIT: Not really using buku for bookmarks
        "buku"
        "bukubrow"
    ];

    nixpkgs.prebuilt.chat.gui =
        let linux = np.pick { linux = "home"; } [
                # REVISIT: 2025-04-12: Unsupported for Darwin
                "irccloud"

                # REVISIT: 2025-03-23: Prebuilt for Linux, but not for Darwin
                "caprine"
                "element-desktop"
                "signal-desktop-bin"
            ];
        in linux;

    nixpkgs.build.chat.gui =
        let all = pickAll "home" [
                "discord"
                "zoom-us"
            ];
            darwin = np.pick { darwin = "home"; } [
                "caprine"
                "element-desktop"
                "signal-desktop-bin"
            ];
            linux = np.pick { linux = "home"; } [
                "slack"
            ];
        in all // darwin // linux;

    nixpkgs.prebuilt.chat.tui = pickAll "home" [
        "slack-term"
    ];

    nixpkgs.prebuilt.documentation =
        let all = pickAll "home" [
                "graphviz"
                "imagemagick"
                "nodePackages.textlint"
                "poppler_utils"
                "proselint"
                "python3Packages.grip"
                "t-rec"
            ];
            linux = np.pick { linux = "home"; } [
                "dia"
                "freemind"
                "gimp"
                "inkscape"
                "libreoffice"  # DESIGN: Sometimes has broken build on Darwin
                "peek"
            ];
        in all // linux;

    nixpkgs.build.finance = pickAll "home" [
        "moneydance"
    ];

    nixpkgs.prebuilt.fonts = pickAll "home" [
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
    ] // (lib.filterAttrs (_: v: lib.isDerivation v) np.nixpkgs.home.nerd-fonts);

    nixpkgs.build.fonts = pickAll "home" [
        "sf-symbols"       # font with application icons
        "sketchybar-font"  # font with application icons
        "symbola"          # another Unicode fallback font
    ];

    nixpkgs.prebuilt.programming.c =
        let all = pickAll "home" [
                "cmake"
            ];
            linux = np.pick { linux = "home"; } [
                "gcc"
            ];
        in all // linux;

    nixpkgs.prebuilt.programming.containers = pickAll "home" [
        "kubectl"
    ];

    nixpkgs.build.programming.containers = pickAll "home" [
    ] // {
        google-cloud-sdk = with np.nixpkgs.home;
            google-cloud-sdk.withExtraComponents [
                google-cloud-sdk.components.gke-gcloud-auth-plugin
            ];
    };

    nixpkgs.prebuilt.programming.db = pickAll "home" [
        "pgformatter"
        "postgresql"
        "schemaspy"
        "sqitchPg"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general = pickAll "home" [
        "aider-chat-full"
        "global"
        "gnumake"
        "nil"
        "nixd"
        "nodejs"  # DESIGN: needed for Cursor remote SSH extension
        "tcount"
        "tokei"
        "wireshark"
    ];

    nixpkgs.build.programming.general = pickAll "master" [
        "code-cursor"
    ];

    nixpkgs.prebuilt.programming.haskell = pickAll "home" [
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

    nixpkgs.prebuilt.programming.java = pickAll "home" [
    ];

    nixpkgs.prebuilt.programming.lua = pickAll "home" [
        "luaformatter"
        "lua5_4"
    ];

    nixpkgs.prebuilt.programming.python = pickAll "home" [
        "python3"
    ];

    nixpkgs.prebuilt.programming.scala = pickAll "home" [
        "sbt-extras"
    ];

    nixpkgs.prebuilt.programming.shell = pickAll "home" [
        "shellcheck"
    ];

    nixpkgs.prebuilt.sync = pickAll "home" [
        "unison"
    ];

    nixpkgs.build.uncategorized.darwin = np.pick { darwin = "home"; } [
        "sketchybar-helpers"
        "sketchybar-lua"
    ];

    nixpkgs.build.uncategorized.linux = np.pick { linux = "home"; } [
    ];

}
