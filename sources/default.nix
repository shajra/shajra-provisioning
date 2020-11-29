let

    srcs = import ./sources.nix;

    lib = (import srcs.nixpkgs { config = {}; overlays = []; }).lib;

    isDarwin = builtins.elem builtins.currentSystem lib.systems.doubles.darwin;

    nixpkgs-stable-linux = srcs.nixpkgs;
    nixpkgs-stable-darwin = srcs.nixpkgs-darwin;
    nixpkgs-stable =
        if isDarwin then nixpkgs-stable-darwin else nixpkgs-stable-linux;

    srcs-merged = srcs // {
        inherit nixpkgs-stable nixpkgs-stable-linux nixpkgs-stable-darwin;
    };

in builtins.removeAttrs srcs-merged ["nixpkgs" "nixpkgs-darwin"]
