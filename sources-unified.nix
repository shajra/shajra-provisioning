let

    srcs = import ./sources.nix;

    lib = (import srcs.nixpkgs { config = {}; }).lib;

    isDarwin = builtins.elem builtins.currentSystem lib.systems.doubles.darwin;

in

    if isDarwin
    then srcs // { nixpkgs-stable = srcs.nixpkgs-stable-darwin; }
    else srcs // { nixpkgs-stable = srcs.nixpkgs-stable-linux ; }
