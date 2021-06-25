{
    buildSet = "all";
    buildInfrastructure = "all";
    hackage.version = {
        apply-refact = "latest";
        ghcid = "latest";
        hlint = "latest";
        stylish-haskell = "latest";
    };

    haskell-nix.checkMaterialization = false;
    haskell-nix.platformSensitive = [
        "ghcid"
    ];
    # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
    haskell-nix.hackage.index = {
        state = "2021-06-24T00:00:00Z";
        sha256 = "fc47b656324984fe18aa6bf5d8fa605f2cadb94d41c9004f2bf8cca014301138";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    nixpkgs = {
        allowUnfree = true;
        ungoogled-chromium = {
            enablePepperFlash = false;
        };
    };
}
