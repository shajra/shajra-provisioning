{
    hackage.version = {
        apply-refact = "0.8.2.1";
        ghcid = "0.8.7";
        hlint = "3.2";
        implicit-hie = "0.1.1.0";
        stylish-haskell = "0.11.0.3";
    };
    haskell-nix.hackage.index = {
        state = "2020-09-27T00:00:00Z";
        sha256 = "0f1llfnmbsd5x1c45imc7sx0hj03j6j3g1a23m9b6f74barx4pw2";
    };
    haskell-nix.nixpkgs-pin = "nixpkgs-2003";
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
    stackage.resolver = "lts-16.16";  # DESIGN: not used currently
}
