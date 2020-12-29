{
    buildSet = "all";
    buildInfrastructure = "all";
    hackage.version = {
        apply-refact = "0.8.2.1";
        ghcid = "0.8.7";
        hlint = "3.2.3";
        stylish-haskell = "0.12.2.0";
    };

    haskell-nix.useMaterialization = true;
    haskell-nix.checkMaterialization = false;
    # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
    haskell-nix.hackage.index = {
        state = "2020-12-28T00:00:00Z";
        sha256 = "ce5696846e316c2d151c69f5f292dfe1aceca540253757831d9081990a2a1d90";
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
    stackage.resolver = "lts-16.27";  # DESIGN: not used currently
}
