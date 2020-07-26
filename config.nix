{
    hackage.version = {
        apply-refact = "0.8.2.0";
        ghcid = "0.8.7";
        ghcide = "0.2.0";
        hlint = "3.1.6";
        stylish-haskell = "0.11.0.0";

        # transitive dependencies for Nixpkgs infrastructure
        cabal-helper = "1.1.0.0";
        haskell-lsp = "0.22.0.0";
        haskell-lsp-types = "0.22.0.0";
        hie-bios = "0.6.1";
    };
    haskell-nix.hackage.index = {
        state = "2020-07-26T00:00:00Z";
        sha256 = "07mk4c7bbqfn47yw5ffaijbrfcza5fvw813vf3i4mr00m3vjjmc8";
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
    stackage.resolver = "lts-16.6";  # DESIGN: not used currently
}
