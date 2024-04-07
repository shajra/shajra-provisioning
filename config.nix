{
    build = {
        dev = false;  # true, to skip Haskell.nix build for a faster dev cycle
    };

    infrastructure = {
        hackage.version = {
            apply-refact    = "latest";
            fast-tags       = "latest";
            ghc-events      = "latest";
            ghcid           = "latest";
            haskdogs        = "latest";
            hasktags        = "latest";
            hlint           = "latest";
            hoogle          = "latest";
            hp2pretty       = "latest";
            stylish-haskell = "latest";
            threadscope     = "latest";
        };
        haskell-nix = {
            checkMaterialization = false;
            platformSensitive = [
                "ghcid"
            ];
            # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
            hackage.index = {
                state = "2024-04-13T00:00:00Z";
                sha256 = "89c7f1d37885b6042a4bd25622bce774c619ceebd8524a1ec4b5532855996d27";
            };
            nixpkgs-pin = "nixpkgs-unstable";
        };
        nixpkgs = {
            allowUnfree = true;
        };
    };

    # Options for nixos-rebuild, darwin-rebuild, home-manager provisioning
    provision = {
        # Which version of Nixpkgs passed to NixOS-style modules as 'pkgs'.
        # Specific versions are specificed in external/sources.json.
        # Options:
        #     - "stable-darwin": nixpkgs-$RELEASE_VERSION-darwin
        #     - "stable-linux": nixos-$RELEASE_VERSION
        #     - "unstable": nixpkgs-unstable
        pkgs = {
            system = {
                darwin = "stable-darwin";
                linux = "stable-linux";
            };
            home = {
                darwin = "unstable";
                linux = "unstable";
            };
        };
        user = {
            cake = rec {
                homeDirectory = "/home/${username}";
                username = "tnks";
            };
            jelly = rec {
                homeDirectory = "/home/${username}";
                username = "tnks";
            };
            hole = rec {
                homeDirectory = "/home/${username}";
                username = "tnks";
            };
            bagel = rec {
                homeDirectory = "/Users/${username}";
                username = "shajra";
            };
            shajra = rec {
                homeDirectory = "/home/${username}";
                username = "shajra";
            };
        };
    };
}
