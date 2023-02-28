inputs: withSystem:
final: prev:

let

    lib = prev.lib;
    hostPlatform = prev.stdenv.hostPlatform;
    system = hostPlatform.system;
    isDarwin = hostPlatform.isDarwin;

    config = import ../config.nix;
    infraConfig = config.infrastructure;
    pkgsConfig = config.provision.pkgs;
    isDevBuild = config.build.dev;

    isDrvSet = s: lib.isAttrs s
        && (lib.any lib.isDerivation (builtins.attrValues s) || s == {});

    joinForCi = name: set:
        let found = deepMerge (lib.collect isDrvSet set);
        in prev.linkFarm "shajra-provision-ci-${name}" found;

    deepMerge = builtins.foldl' (acc: a: lib.recursiveUpdate acc a) {};

    deepDrvSetToList = lib.mapAttrsRecursiveCond
        (s: ! isDrvSet s)
        (_path: s: builtins.attrValues s);

    build = withSystem system ({ inputs', nixpkgs, ... }: rec {
        np = final.callPackage ./nixpkgs {
            inherit infraConfig inputs inputs' isDarwin nixpkgs pkgsConfig system;
        };
        hn = final.callPackage ./haskell-nix {
            inherit infraConfig inputs isDarwin system;
        };
        pkgs = final.callPackage ../packages.nix {
            inherit inputs isDarwin isDevBuild np hn;
        };
    });

    pkgs.sets = deepMerge [
        build.pkgs.nixpkgs.prebuilt
        build.pkgs.nixpkgs.build
        build.pkgs.haskell-nix.prebuilt
        build.pkgs.haskell-nix.build
        build.pkgs.shajra.build
    ];

    pkgs.lists = deepDrvSetToList pkgs.sets;

    ci.prebuilt.nixpkgs     = joinForCi "prebuilt-nixpkgs" build.pkgs.nixpkgs.prebuilt;
    ci.prebuilt.haskell-nix = joinForCi "prebuilt-haskellnix" build.pkgs.haskell-nix.prebuilt;
    ci.prebuilt.shajra      = joinForCi "prebuilt-shajra" build.pkgs.shajra.prebuilt;
    ci.build.nixpkgs        = joinForCi "build-nixpkgs" build.pkgs.nixpkgs.build;
    ci.build.haskell-nix    = joinForCi "build-haskellnix" build.pkgs.haskell-nix.build;
    ci.build.shajra         = joinForCi "build-shajra" build.pkgs.shajra.build;
    ci.all                  = joinForCi "all" build.pkgs;

in {
    shajra-provision = {
        infra = { inherit (build) np hn; };
        inherit config pkgs ci;
    };
}
