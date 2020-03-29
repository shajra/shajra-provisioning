{
    hackage.version = {
        # DESIGN: apply-refact 0.7 is only for GHC 8.8.x
        # DESIGN: apply-refact 0.7 won't build until Cabal 3.0.1.0 on Hackage
        apply-refact = "0.6.0.0";
        ghcid = "0.8.5";
        ghcide = "0.1.0";
        haskell-ci = "0.8";
        hlint = "2.2.11";
        stylish-haskell = "0.11.0.0";

        # Auxiliary packages for Nixpkgs build
        cabal-helper = "1.0.0.0";
        haskell-lsp = "0.20.0.1";
        haskell-lsp-types = "0.20.0.0";
        hie-bios = "0.4.0";
    };
    haskell-nix.hackage.index = {
        state = "2020-03-21T00:00:00Z";
        sha256 = "0z5yp7p2671y87801m1ilia0f8i8vwx400yhw2xr4ljfv4ybx17d";
    };
    haskell-nix.nixpkgs-pin = "release-19.09";
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
            enableGoogleTalkPlugin = false;
            enableAdobeFlash = false;
        };
    };
    stackage.resolver = "lts-15.3";  # DESIGN: not used currently
}
