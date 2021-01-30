{
    buildSet = "all";
    buildInfrastructure = "all";
    hackage.version = {
        apply-refact = "0.9.0.0";
        ghcid = "0.8.7";
        hlint = "3.2.7";
        stylish-haskell = "0.12.2.0";
    };

    haskell-nix.checkMaterialization = false;
    haskell-nix.platformSensitive = [
        "ghcid"
    ];
    # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
    haskell-nix.hackage.index = {
        state = "2021-01-30T00:00:00Z";
        sha256 = "fa890b301aa2e30eed8a0a62e87210eeb3179d97f184e5aaa338516cc07a8e4a";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    nixpkgs = {
        allowUnfree = true;
        chromium = {
            enablePepperFlash = false;
        };
        firefox = {
            enableAdobeFlash = false;
        };
    };
    stackage.resolver = "lts-17.0";  # DESIGN: not used currently
}
