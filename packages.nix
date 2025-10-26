{
  lib,
  np,
  inputs',
  isDevBuild,
}:

let

  ghc = "ghc9103";

  when = lib.optionalAttrs;

  pickAll =
    nixpkgsName:
    np.pick {
      darwin = nixpkgsName;
      linux = nixpkgsName;
    };

in
rec {

  shajra.prebuilt = { };

  shajra.build.audio.gui = {
    inherit (inputs'.bluos-nix.packages) bluos-controller;
  };

  nixpkgs.prebuilt.audio.tui =
    let
      all = pickAll "home" [
        "mpc_cli"
        "whipper"
      ];
      linux = np.pick { linux = "home"; } [
        "playerctl"
        "ponymix"
        "pulsemixer"
      ];
    in
    all // linux;

  nixpkgs.prebuilt.base.gui =
    let
      # DESIGN: these are all Mac-only applications; unstable is fine
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
    in
    darwin // linux // nixpkgs.prebuilt.fonts;

  nixpkgs.build.base.gui =
    let
      all = pickAll "home" [
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
        "i3-scratchpad"
        "i3status-rust-dunst"
        "i3-workspace-name"
      ];
    in
    all // darwin // linux // nixpkgs.build.fonts;

  nixpkgs.prebuilt.base.tui =
    let
      all = pickAll "home" [
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
        "gnupg" # TODO: home-manager
        "languagetool"
        "libxml2" # for xmllint
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
    in
    all // darwin // linux;

  nixpkgs.build.base.tui =
    let
      all.picked = pickAll "home" [
        "home-manager-latest"
        "preview-file"
      ];
      all.aspell = np.nixpkgs.home.aspellWithDicts (
        d: with d; [
          en
          en-computers
          en-science
        ]
      );
    in
    all.picked // { inherit (all) aspell; };

  nixpkgs.prebuilt.centralized = np.pick { linux = "home"; } [
    "ente-desktop"
    "ente-cli"
  ];

  nixpkgs.prebuilt.chat.gui =
    let
      all = pickAll "home" [
        # REVISIT: 2025-10-12: element-desktop probably will cache soon
        "element-desktop"
      ];
      linux = np.pick { linux = "home"; } [
        "irccloud" # REVISIT: 2025-04-12: Unsupported for Darwin
        "caprine"
        "signal-desktop-bin"
      ];
    in
    all // linux;

  nixpkgs.build.chat.gui =
    let
      all = pickAll "home" [
        "discord"
        "zoom-us"
      ];
      darwin = np.pick { darwin = "home"; } [
        "caprine"
        "signal-desktop-bin"
      ];
      linux = np.pick { linux = "home"; } [
        "slack"
      ];
    in
    all // darwin // linux;

  nixpkgs.prebuilt.chat.tui = pickAll "home" [
    "slack-term"
  ];

  nixpkgs.prebuilt.documentation =
    let
      all = pickAll "home" [
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
        "libreoffice"
        "peek"
      ];
    in
    all // linux;

  nixpkgs.build.finance = pickAll "home" [
    "moneydance"
  ];

  nixpkgs.prebuilt.fonts =
    pickAll "home" [
      "emacs-all-the-icons-fonts" # for Emacs, used automatically by Doom
      "etBook" # stylish font from Edward Tufte's books
      "fira" # variable font to complement Fira Code
      "font-awesome_6" # for i3status-rust icons
      "freefont_ttf" # a Unicode fallback font
      "hasklig" # font with Haskell ligatures
      "noto-fonts-emoji" # popular font, might like it
      "noto-fonts-extra" # popular font, might like it
      "noto-fonts" # popular font, might like it
      "source-serif" # serif font to complement Sauce Code Pro
    ]
    // (lib.filterAttrs (_: v: lib.isDerivation v) np.nixpkgs.home.nerd-fonts);

  nixpkgs.build.fonts = pickAll "home" [
    "sf-symbols" # font with application icons
    "sketchybar-font" # font with application icons
    "symbola" # another Unicode fallback font
  ];

  nixpkgs.prebuilt.programming.c =
    let
      all = pickAll "home" [
        "clang-tools" # for clang-format
        "cmake"
      ];
      linux = np.pick { linux = "home"; } [
        "gcc"
      ];
    in
    all // linux;

  nixpkgs.prebuilt.programming.cloud = pickAll "home" [
    "dockfmt"
    "kubectl"
  ];

  nixpkgs.build.programming.cloud =
    pickAll "home" [
      "opentofu"
    ]
    // {
      google-cloud-sdk =
        with np.nixpkgs.home;
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
    "codex"
    "global"
    "gnumake"
    "nil"
    "nixd"
    "nodejs" # DESIGN: needed for Cursor remote SSH extension
    "plantuml"
    "tcount"
    "tokei"
    "wireshark"
  ];

  nixpkgs.build.programming.general = pickAll "home" [
    "code-cursor"
  ];

  nixpkgs.prebuilt.programming.go = pickAll "home" [
    "delve"
    "go"
    "gocode-gomod"
    "golangci-lint"
    "gomodifytags"
    "gopkgs"
    "gopls"
    "gore"
    "goreleaser"
    "gotools"
  ];

  nixpkgs.prebuilt.programming.haskell = pickAll "home" [
    "cabal2nix"
    "cabal-install"
    "stack"
    "haskell.compiler.${ghc}"
    "haskell.packages.${ghc}.cabal-fmt"
    "haskell.packages.${ghc}.djinn"
    "haskell.packages.${ghc}.eventlog2html"
    "haskell.packages.${ghc}.fast-tags"
    "haskell.packages.${ghc}.ghc-events"
    "haskell.packages.${ghc}.ghcid"
    "haskell.packages.${ghc}.haskdogs"
    "haskell.packages.${ghc}.haskell-language-server"
    "haskell.packages.${ghc}.hasktags"
    #REVISIT: 2025-10-12: HLint incompatible with 9.10; wait for 9.12
    #"haskell.packages.${ghc}.hlint"
    "haskell.packages.${ghc}.hoogle"
    "haskell.packages.${ghc}.hp2pretty"
    "haskell.packages.${ghc}.profiterole"
    "haskell.packages.${ghc}.profiteur"
    "haskell.packages.${ghc}.stylish-haskell"
  ];

  haskell-nix.prebuilt.programming.haskell = {
    # DESIGN: don't use enough to want to think about a cache miss
    #nix-tools = hn.nixpkgs.haskell-nix.nix-tools."${ghc}";
  };

  haskell-nix.build.programming.haskell = when (!isDevBuild) { }
  # DESIGN: Nixpkgs-built binaries above are fine (maybe bloated)
  #({}
  #// (hn.fromHackage ghc "apply-refact")
  #// (hn.fromHackage ghc "fast-tags")
  #// (hn.fromHackage ghc "ghc-events")
  #// (hn.fromHackage ghc "ghcid")
  #// (hn.fromHackage ghc "haskdogs")
  #// (hn.fromHackage ghc "hasktags")
  #// (hn.fromHackage ghc "hlint")
  #// (hn.fromHackage ghc "hoogle")
  #// (hn.fromHackage ghc "hp2pretty")
  #// (hn.fromHackage ghc "threadscope")
  #// (hn.fromHackageCustomized ghc "stylish-haskell" { configureArgs = "-f ghc-lib"; })
  #)
  ;

  shajra.build.programming.haskell =
    let
      tags = import inputs'.haskell-tags-nix;
    in
    when false {
      #in when (! isDevBuild) {
      haskell-hls-tags = tags.haskell-tags-nix-exe;
    };

  nixpkgs.prebuilt.programming.java = pickAll "home" [
    "clang-tools" # DESIGN: Doom Java module wants clang-format
  ];

  nixpkgs.prebuilt.programming.lua = pickAll "home" [
    #"luaformatter"  # REVISIT: 2024-10-12: build broken
    "lua5_4"
  ];

  nixpkgs.prebuilt.programming.python = pickAll "home" [
    "pipenv"
    "python3"
    "python3Packages.black"
    "python3Packages.isort"
    "python3Packages.pyflakes"
    "python3Packages.pytest"
    "python3Packages.setuptools"
  ];

  # REVISIT: 2025-06-07: Unsupported for Darwin
  nixpkgs.prebuilt.programming.racket = np.pick { linux = "home"; } [
    "racket"
  ];

  nixpkgs.prebuilt.programming.ruby = pickAll "home" [
    "ruby"
  ];

  nixpkgs.prebuilt.programming.rust = pickAll "home" [
    "cargo"
    "cargo-udeps"
    "clippy"
    "rust-analyzer"
    "rustc"
    "rustfmt"
  ];

  nixpkgs.prebuilt.programming.scala = pickAll "home" [
    "sbt-extras"
    "scalafmt"
  ];

  nixpkgs.prebuilt.programming.shell = pickAll "home" [
    "shellcheck"
    "shfmt"
  ];

  nixpkgs.prebuilt.programming.web = pickAll "home" [
    "html-tidy"
    "stylelint"
    "nodePackages.js-beautify"
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
