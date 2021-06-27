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
        state = "2021-06-26T00:00:00Z";
        sha256 = "2cb63022b8930ae97189550b75ccfc9bab6b30e1573e77a686007513efb577fd";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    nixpkgs = {
        allowUnfree = true;
        ungoogled-chromium = {
            enablePepperFlash = false;
        };
    };
}
