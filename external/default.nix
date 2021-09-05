{ config ? import ../config.nix
, externalOverrides ? {}
}:

let

    srcs = import ./sources.nix // externalOverrides;
    cfg = config.provision.pkgs;

    nixpkgs-bootstrap = import srcs.nixpkgs { config = {}; overlays = []; };
    isDarwin = nixpkgs-bootstrap.stdenv.isDarwin;

    nixpkgs-stable-darwin = srcs.nixpkgs-darwin;
    nixpkgs-stable-linux = srcs.nixpkgs;
    nixpkgs-stable =
        if isDarwin then nixpkgs-stable-darwin else nixpkgs-stable-linux;

    lookup = {
        "stable-darwin" = nixpkgs-stable-darwin;
        "stable-linux" = nixpkgs-stable-linux;
        "unstable" = srcs.nixpkgs-unstable;
    };

    nixpkgs-home =
        if isDarwin
        then lookup."${cfg.home.darwin}"
        else lookup."${cfg.home.linux}";

    nixpkgs-system =
        if isDarwin
        then lookup."${cfg.system.darwin}"
        else lookup."${cfg.system.linux}";

    srcs-merged = srcs // {
        inherit
        nixpkgs-stable
        nixpkgs-stable-darwin
        nixpkgs-stable-linux
        nixpkgs-home
        nixpkgs-system;
    };

in builtins.removeAttrs srcs-merged ["nixpkgs" "nixpkgs-darwin"]
