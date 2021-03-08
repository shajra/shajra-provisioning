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
        state = "2021-05-22T00:00:00Z";
        sha256 = "a154e09d3065552f413f83de105a230a3655f7f91058277a87b8d37ac1698587";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    nixpkgs = {
        allowUnfree = true;
        ungoogled-chromium = {
            enablePepperFlash = false;
        };
    };
}
