{ buildSet ? "all"
, buildInfrastructure ? "all"
, useMaterialization ? false
, checkMaterialization ? false
}:

let

    infra = import ./infrastructure {
        inherit checkMaterialization useMaterialization;
        config = import ./config.nix;
        sources = import ./sources.nix;
    };

    pkgs = import ./packages.nix infra;

    includeSet = bs: buildSet == bs || buildSet == "all";
    includeInfra = i: buildInfrastructure == i || buildInfrastructure == "all";
    include = bs: i: pkgs:
        if includeSet bs && includeInfra i then pkgs else {};

in
    (   include "prebuilt" "nixpkgs"     pkgs.prebuilt.nixpkgs)
    // (include "prebuilt" "haskell-nix" pkgs.prebuilt.haskell-nix)
    // (include "build"    "nixpkgs"     pkgs.build.nixpkgs)
    // (include "build"    "haskell-nix" pkgs.build.haskell-nix)
