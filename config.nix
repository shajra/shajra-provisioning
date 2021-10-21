{
    # Which packages to build and include
    # DESIGN: to build sets in parellel in CI
    build = {
        # Options:
        #     - "all": include everything
        #     - "prebuilt": limit to packages expected prebuilt/cached
        #     - "build": limit to packages expected fresh builds
        set = "all";
        # Options:
        #     - "all": include all infrastructure
        #     - "nixpkgs": limit to packages from Nixpkgs
        #     - "haskell-nix": limit to packages built with Haskell.nix
        #     - "shajra": limit to https://github.com/shajra projects
        infrastructure = "all";
        dev = false;  # true, to skip Haskell.nix build for a faster dev cycle
    };

    infrastructure = {
        hackage.version = {
            apply-refact = "latest";
            ghcid = "latest";
            hlint = "latest";
            stylish-haskell = "latest";
        };
        haskell-nix = {
            checkMaterialization = false;
            platformSensitive = [
                "ghcid"
            ];
            # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
            hackage.index = {
                state = "2021-10-21T00:00:00Z";
                sha256 = "b9c245e69a901f25e7eb98742f510490dd8c1c2704f70f3b806bf01289059cd0";
            };
            nixpkgs-pin = "nixpkgs-2105";
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
                darwin = "stable-darwin";
                linux = "unstable";
            };
        };
    };
}
