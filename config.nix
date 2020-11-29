{
    buildSet = "all";
    buildInfrastructure = "all";
    hackage.version = {
        apply-refact = "0.8.2.1";
        ghcid = "0.8.7";
        hlint = "3.2.3";
        implicit-hie = "0.1.2.4";
        stylish-haskell = "0.12.2.0";
    };

    haskell-nix.useMaterialization = true;
    haskell-nix.checkMaterialization = false;
    haskell-nix.hackage.index = {
        state = "2020-11-29T00:00:00Z";
        sha256 = "779b3ad819554991b8f9d51fe518b28fa35b8d351bb289b81a0ab03c3c429d5f";
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
    stackage.resolver = "lts-16.23";  # DESIGN: not used currently
}
