{
    hackage.version = {
        apply-refact = "0.8.2.1";
        ghcid = "0.8.7";
        hlint = "3.2";
        implicit-hie = "0.1.1.0";
        stylish-haskell = "0.12.1.0";
    };
    haskell-nix.hackage.index = {
        state = "2020-10-05T00:00:00Z";
        sha256 = "1sazxknvajhyb6v8xhmi0ajgnz5jz300iww75gh5x8xnaf7lhfvw";
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
    stackage.resolver = "lts-16.17";  # DESIGN: not used currently
}
