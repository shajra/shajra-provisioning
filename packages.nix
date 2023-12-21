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
        "microsoft-edge-beta"
        "pavucontrol"
        "simple-scan"
        "sxiv"
        "xclip"
        "xorg.xdpyinfo"
        "xorg.xev"
        "zoom-us"

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
        "nixfmt"
        "paperkey"
        "patchelf"
        "procps"
        "pstree"
        "ripgrep"
        "rsync"
        "scc"
        "tmux"
        "tree"
        "unzip"
        "wemux"
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
        "libreoffice"  # DESIGN: 2022-04-15: broke for Darwin
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
        "schemaspy"
        "sqlint"
        "sqlite"
    ];

    nixpkgs.prebuilt.programming.general = pickHome [
        "gnumake"
    ];

    nixpkgs.prebuilt.programming.haskell = pickHome [
        "cabal2nix"
        "cabal-install"
        "stack"
        "haskell.compiler.ghc963"
        "haskell.packages.ghc963.apply-refact"
        # "haskell.packages.ghc963.djinn"  # DESIGN: 2023-12-20: broken
        "haskell.packages.ghc963.fast-tags"
        "haskell.packages.ghc963.ghc-events"
        "haskell.packages.ghc963.ghcid"
        "haskell.packages.ghc963.haskdogs"
        "haskell.packages.ghc963.hasktags"
        "haskell.packages.ghc963.hoogle"
        "haskell.packages.ghc963.hlint"
        "haskell.packages.ghc963.hp2pretty"
        "haskell.packages.ghc963.stylish-haskell"
    ];

    nixpkgs.prebuilt.programming.java = pickHome [
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
        # DESIGN: Nixpkgs seems to have given up on Yabai and Skhd
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

    nixpkgs.build.programming.haskell = {}
        # DESIGN: not cached; building Haskell tools with Haskell.nix
        ;

    nixpkgs.build.unused.darwin = np.pick { darwin = "home"; } [
        # DESIGN: emacsMacport broken for M1
        # https://github.com/NixOS/nixpkgs/issues/127902
        # DESIGN: note emacsMacport doesn't have native compilation
        # https://github.com/railwaycat/homebrew-emacsmacport/issues/274
        #"emacsMacport"
    ];

    nixpkgs.build.unused.linux = np.pick { linux = "home"; } [
        # DESIGN: https://github.com/doomemacs/doomemacs#prerequisites
        # using emacs29 over emacs-unstable to hit Nixpkgs cache
        "emacs29"
    ];

    haskell-nix.prebuilt.programming.haskell = {
        # DESIGN: don't use enough to want to think about a cache miss
        #nix-tools = hn.nixpkgs.haskell-nix.nix-tools.ghc963;
    };

    haskell-nix.build.programming.haskell = when (! isDevBuild) (
        {}
        # DESIGN: Nixpkgs-built binaries above are fine (maybe bloated)
        #// (hn.fromHackage "ghc963" "apply-refact")
        #// (hn.fromHackage "ghc963" "fast-tags")
        #// (hn.fromHackage "ghc963" "ghc-events")
        #// (hn.fromHackage "ghc963" "ghcid")
        #// (hn.fromHackage "ghc963" "haskdogs")
        #// (hn.fromHackage "ghc963" "hasktags")
        #// (hn.fromHackage "ghc963" "hlint")
        #// (hn.fromHackage "ghc963" "hoogle")
        #// (hn.fromHackage "ghc963" "hp2pretty")
        #// (hn.fromHackage "ghc963" "threadscope")
        #// (hn.fromHackageCustomized "ghc963" "stylish-haskell" { configureArgs = "-f ghc-lib"; })

        # DESIGN: marked broken in Nixpkgs, doesn't seem to build with
        # Haskell.nix either (need to look for a modern alternative exists)
        #// (hn.fromHackage "ghc963" "ghc-events-analyze")
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
