{ inputs
, inputs'
}:

let

    # DESIGN: downloading the latest hashes is time-consuming
    #overlay.all-cabal-hashes = final: prev: {
    #    all-cabal-hashes = sources.all-cabal-hashes;
    #};

    external.emacs  = inputs.emacs-overlay.overlays.default;
    external.nur    = inputs.nur.overlays.default;
    external.vscode = inputs.vscode-overlay.overlays.default;

    external.sources = final: prev: {
        sources = prev.sources or {} // {
            inherit (inputs)
                delta
                dircolors-solarized
                direnv-nix-lorelei
                dotfiles-felixkratz
                fzf-fish
                jankyborders
                kaleidoscope
                kitty-scrollback-nvim
                luaposix
                lieer
                moneydance
                pointless-xcompose
                sf-symbols
                sketchybar
                sketchybar-font-dist
                sketchybar-font-src
                sketchybar-lua
                xcompose;
        };
    };

    external.packages = final: prev: {
        home-manager-latest = inputs'.home-manager.packages.default;
        nix-project-lib = inputs'.nix-project.legacyPackages.lib.scripts;
        inherit (inputs'.nix-project.packages) org2gfm;
    };

    external.modules = final: prev: {
        homeModules.vscode-server =
            inputs.vscode-server.homeModules.default;
    };

    internal.sources = final: prev:
        let rejectFile = path: type: regex:
                type != "regular" || builtins.match regex path == null;
            rejectDir = path: type: regex:
                type != "directory" || builtins.match regex path == null;
        in {
            sources = prev.sources or {} // {
                shajra-provisioning = builtins.path {
                    path = ../..;
                    name = "shajra-provisioning";
                    filter = path: type:
                        (rejectFile path type ".*[.](md|org)")
                        && (rejectDir path type "[.]git")
                        && (rejectDir path type "[.]github")
                        && (rejectFile path type "result.*");
                };
            };
        };

    internal.overlays =
        builtins.attrValues
            (builtins.mapAttrs
                (p: t: import (./overlays + "/${p}"))
                (builtins.readDir ./overlays));

    internal.packages = final: prev:
        builtins.mapAttrs
            (p: t: final.callPackage (./packages + "/${p}") {})
            (builtins.readDir ./packages);

in  [
    external.emacs
    external.nur
    external.vscode
    external.sources
    external.modules
    external.packages
] ++ internal.overlays ++ [
    internal.sources
    internal.packages
]
