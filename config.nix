{
    buildSet = "all";
    buildInfrastructure = "all";
    hackage.version = {
        apply-refact = "0.9.0.0";
        ghcid = "0.8.7";
        hlint = "3.2.7";
        stylish-haskell = "0.12.2.0";
    };

    haskell-nix.useMaterialization = true;
    haskell-nix.checkMaterialization = false;
    # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
    haskell-nix.hackage.index = {
        state = "2021-01-14T00:00:00Z";
        sha256 = "15dfa7916a43118b182ca424330b6a2a5d9d7e7344cc9106196c867972c634f9";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2009";
    haskell-nix.plan = {
        # DESIGN: a "<packagename>.sha256" property will set the plan's hash
        # DESIGN: a "<packagename>.check" property will check materialization
        # IDEA: when ready: https://github.com/digital-asset/ghcide/issues/113
        #ghcide.sha256 = "0000000000000000000000000000000000000000000000000000";
        #ghcide.check = true;
    };
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
