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
        state = "2021-02-20T00:00:00Z";
        sha256 = "74b361a53bd41e1378fceb274b76a461b2e9791fec8bc61aa0b5768d5c930360";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    nixpkgs = {
        allowUnfree = true;
        ungoogled-chromium = {
            enablePepperFlash = false;
        };
    };
}
