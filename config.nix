{
    build = {
        dev = false;  # true, to skip Haskell.nix build for a faster dev cycle
    };

    infrastructure = {
        hackage.version = {
            apply-refact    = "latest";
            fast-tags       = "latest";
            ghcid           = "latest";
            hlint           = "latest";
            haskdogs        = "latest";
            hasktags        = "latest";
            hp2pretty       = "latest";
            stylish-haskell = "latest";
        };
        haskell-nix = {
            checkMaterialization = false;
            platformSensitive = [
                "ghcid"
            ];
            # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
            hackage.index = {
                state = "2023-07-03T00:00:00Z";
                sha256 = "6702d0f48ed8e69e51bd6d10f1996add0d2ddc1ff580da76d6eef813e52cded8";
            };
            nixpkgs-pin = "nixpkgs-unstable";
        };
        nixpkgs = {
            allowUnfree = true;
            ungoogled-chromium = {
                enablePepperFlash = false;
            };
            firefox = {
                enableTridactylNative = true;
            };
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
