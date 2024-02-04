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
                state = "2024-02-04T00:00:00Z";
                sha256 = "0f703fb20c5d0cbddae0e828614e23e4cfd8de6dc97c1006a640afdd2c4947b4";
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
