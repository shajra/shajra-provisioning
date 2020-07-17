{ np
, hn
, isDarwin
}:

let

    attrPaths.common = [
        "autojump"
        "bzip2"
        "cabal2nix"
        "cabal-install"
        "cachix"
        "cloc"
        "coreutils"
        "dhall"
        "direnv"
        "ghc"
        "gitFull"
        "global"
        "gnugrep"
        "gnumake"
        "gnupg"
        "graphviz"
        "htop"
        "imagemagick"
        "ispell"
        "jq"
        "jre"
        "mpc_cli"
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

    attrPaths.darwin = [
        "mongodb"
        "mongodb-tools"
        "postgresql_9_5"
        "vim"
    ];

    attrPaths.linux = [
        "binutils"
        "chromium"
        "dfu-programmer"
        "dfu-util"
        "emacs"
        "escrotum"
        "feh"
        "firefoxWrapper"
        "freemind"
        "fswatch"
        "inkscape"
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
    ];

    nixpkgs.linux.extra = with np.nixpkgs-unstable; {
        texlive = texlive.combine {
            inherit (texlive) scheme-medium;
        };
    };

    nixpkgs.darwin.all = {}
        // (np.pick "stable" attrPaths.common)
        // (np.pick "stable" attrPaths.darwin)
        ;

    nixpkgs.linux.all = {}
        // (np.pick "unstable" attrPaths.common)
        // (np.pick "unstable" attrPaths.linux)
        // nixpkgs.linux.extra
        ;

in

{
    prebuilt.nixpkgs =
        if isDarwin
        then nixpkgs.darwin.all
        else nixpkgs.linux.all;

    prebuilt.haskell-nix = with hn.haskell-nix; {
        inherit nix-tools;
    };

    build.nixpkgs = {}

        // (np.hs.fromPackages "unstable" "ghc883" "djinn")
        // (np.hs.fromPackages "unstable" "ghc883" "fast-tags")
        // (np.hs.fromPackages "unstable" "ghc883" "ghc-events")
        // (np.hs.fromPackages "unstable" "ghc883" "haskdogs")
        // (np.hs.fromPackages "unstable" "ghc883" "hasktags")
        // (np.hs.fromPackages "unstable" "ghc883" "hoogle")
        // (np.hs.fromPackages "unstable" "ghc883" "hp2pretty")
        // (np.hs.fromPackages "unstable" "ghc883" "threadscope")

        // (np.hs.fromPackages "unstable" "ghc865" "pointfree")  # broken for 8.8.3 & 8.10.1, 20-6-14

        #// (hs.np.fromPackages "unstable" "ghc883" "pointful")            # marked broken, 20-6-14
        #// (hs.np.fromPackages "unstable" "ghc883" "ghc-events-analyze")  # marked broken, 20-6-14
        ;

    build.haskell-nix = {}
        // (hn.fromHackage "ghc8101" "apply-refact")
        // (hn.fromHackage "ghc8101" "ghcid")
        // (hn.fromHackage "ghc8101" "hlint")
        // (hn.fromHackage "ghc8101" "stylish-haskell")
        // (hn.fromSource  "ghc8101" "codex")
        ;

}
