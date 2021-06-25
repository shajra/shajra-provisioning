let config = import ./config.nix;
    sources = import ./sources;
in

{ buildSet ? config.buildSet
, buildInfrastructure ? config.buildInfrastructure
, checkMaterialization ? config.haskell-nix.checkMaterialization
}:

let

    infra = import ./infrastructure {
        inherit config checkMaterialization sources;
    };

    userPkgs = import ./packages.nix infra;

    includeSet = bs: buildSet == bs || buildSet == "all";
    includeInfra = i: buildInfrastructure == i || buildInfrastructure == "all";
    include = bs: i: ps:
        if includeSet bs && includeInfra i then ps else {};

    pkgs =
        (   include "prebuilt" "nixpkgs"     userPkgs.prebuilt.nixpkgs)
        // (include "prebuilt" "haskell-nix" userPkgs.prebuilt.haskell-nix)
        // (include "build"    "nixpkgs"     userPkgs.build.nixpkgs)
        // (include "build"    "haskell-nix" userPkgs.build.haskell-nix)
        // (include "build"    "shajra"      userPkgs.build.shajra);

in {
    inherit infra pkgs;
}
