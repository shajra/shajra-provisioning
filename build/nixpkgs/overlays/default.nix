{ inputs
, inputs'
}:

let

    # DESIGN: downloading the latest hashes is time-consuming
    #overlay.all-cabal-hashes = self: super: {
    #    all-cabal-hashes = sources.all-cabal-hashes;
    #};

    external.emacs = inputs.emacs-overlay.overlays.default;

    external.nur   = inputs.nur.overlay;

    external.sources = self: super: {
        sources = super.sources or {} // {
            inherit (inputs)
                colored_man_pages-fish
                delta
                dircolors-solarized
                direnv-nix-lorelei
                fzf-fish
                moneydance
                pointless-xcompose
                xcompose;
        };
    };

    external.packages = self: super: {
        home-manager-latest = inputs'.home-manager.packages.default;
        nix-project-lib = inputs'.nix-project.legacyPackages.lib.scripts;
        org2gfm = inputs'.nix-project.packages.org2gfm;
    };

    internal.sources = self: super:
        let rejectFile = path: type: regex:
                type != "regular" || builtins.match regex path == null;
            rejectDir = path: type: regex:
                type != "directory" || builtins.match regex path == null;
        in {
            sources = super.sources or {} // {
                shajra-provisioning = builtins.path {
                    path = ../../..;
                    name = "shajra-provisioning";
                    filter = path: type:
                        (rejectFile path type ".*[.](md|org)")
                        && (rejectDir path type "[.]git")
                        && (rejectDir path type "[.]github")
                        && (rejectFile path type "result.*");
                };
            };
        };

    internal.overlay = self: super:
        super.lib.mapAttrs
            (p: t: import (./. + "/${p}") self super)
            (builtins.readDir ./.);

    internal.packages = import ../packages/overlay.nix;

in [
    external.emacs
    external.nur
    external.sources
    external.packages
    internal.overlay
    internal.sources
    internal.packages
]
