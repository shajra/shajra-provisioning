inputs: withSystem: final: prev:

let

  inherit (prev) lib;
  inherit (prev.stdenv) hostPlatform;
  inherit (hostPlatform) system;
  inherit (hostPlatform) isDarwin;

  config = import ../config.nix;
  infraConfig = config.infrastructure;
  pkgsConfig = config.provision.pkgs;
  isDevBuild = config.build.dev;

  # REVISIT: Splitting large link farms for known issue with Darwin sandboxes.
  # https://github.com/NixOS/nix/issues/4119
  base32Chars = "0123456789abcdfghijklmnpqrsvwxyz";
  isInSection =
    section: drv:
    let
      hashChar = builtins.substring 11 1 drv.outPath;
    in
    lib.elem hashChar (lib.stringToCharacters section);
  isSplit1 = isInSection (builtins.substring 0 8 base32Chars);
  isSplit2 = isInSection (builtins.substring 8 8 base32Chars);
  isSplit3 = isInSection (builtins.substring 16 8 base32Chars);
  isSplit4 = isInSection (builtins.substring 24 8 base32Chars);
  isAny = _drv: true;

  isDrvSet = s: lib.isAttrs s && (lib.any lib.isDerivation (builtins.attrValues s) || s == { });

  deepMerge = builtins.foldl' (acc: a: lib.recursiveUpdate acc a) { };

  joinForCi =
    name: filter: set:
    let
      found = deepMerge (lib.collect isDrvSet set);
      filtered = lib.filterAttrs (_n: filter) found;
    in
    prev.linkFarm "shajra-provision-ci-${name}" filtered;

  deepDrvSetToList = lib.mapAttrsRecursiveCond (s: !isDrvSet s) (_path: s: builtins.attrValues s);

  build = withSystem system (
    { inputs', nixpkgs, ... }:
    rec {
      np = final.callPackage ./nixpkgs {
        inherit
          infraConfig
          inputs
          inputs'
          isDarwin
          nixpkgs
          pkgsConfig
          system
          ;
      };
      hn = final.callPackage ./haskell-nix {
        inherit
          infraConfig
          inputs
          isDarwin
          system
          ;
      };
      pkgs = final.callPackage ../packages.nix {
        inherit
          inputs'
          isDevBuild
          np
          ;
      };
    }
  );

  pkgs.sets = deepMerge [
    build.pkgs.nixpkgs.prebuilt
    build.pkgs.nixpkgs.build
    build.pkgs.haskell-nix.prebuilt
    build.pkgs.haskell-nix.build
    build.pkgs.shajra.build
  ];

  pkgs.lists = deepDrvSetToList pkgs.sets;

  checkCaching =
    name: sets: checkCacheHit:
    let
      found = builtins.attrValues (deepMerge (lib.collect isDrvSet sets));
      bashOutPaths = lib.concatMapStringsSep " " (d: d.outPath) found;
    in
    prev.writeShellApplication {
      name = "check-caching-${name}";
      inheritPath = false;
      runtimeInputs = [
        # DESIGN: Don't understand, but prev.coreutils leads to cache miss
        final.coreutils
        prev.curl
      ];
      text = ''
        set -eu
        set -o pipefail
        OUT_PATHS=(${bashOutPaths})
        EXIT_CODE=0
        echo "${
          if checkCacheHit then
            "Checking prebuilt packages cached..."
          else
            "Checking packages to build uncached..."
        }"
        for path in "''${OUT_PATHS[@]}"
        do
            PATH_PREFIX="''${path%%-*}"
            HASH="$(basename "$PATH_PREFIX")"
            if ${if checkCacheHit then "!" else ""} curl --fail --silent --head \
                "https://cache.nixos.org/$HASH.narinfo" >/dev/null
            then
                echo "${if checkCacheHit then "NOT" else ""} FOUND: $path"
                EXIT_CODE=1
            fi
        done
        exit "$EXIT_CODE"
      '';
    };

  checkPrebuilt =
    let
      sets = deepMerge [
        build.pkgs.nixpkgs.prebuilt
        build.pkgs.haskell-nix.prebuilt
        build.pkgs.shajra.prebuilt
      ];
    in
    checkCaching "prebuilt" sets true;

  checkBuild =
    let
      sets = deepMerge [
        build.pkgs.nixpkgs.build
        build.pkgs.haskell-nix.build
        build.pkgs.shajra.build
      ];
    in
    checkCaching "build" sets false;

  ci.prebuilt.nixpkgs.split1 = joinForCi "prebuilt-nixpkgs" isSplit1 build.pkgs.nixpkgs.prebuilt;
  ci.prebuilt.nixpkgs.split2 = joinForCi "prebuilt-nixpkgs" isSplit2 build.pkgs.nixpkgs.prebuilt;
  ci.prebuilt.nixpkgs.split3 = joinForCi "prebuilt-nixpkgs" isSplit3 build.pkgs.nixpkgs.prebuilt;
  ci.prebuilt.nixpkgs.split4 = joinForCi "prebuilt-nixpkgs" isSplit4 build.pkgs.nixpkgs.prebuilt;
  ci.prebuilt.haskell-nix = joinForCi "prebuilt-haskellnix" isAny build.pkgs.haskell-nix.prebuilt;
  ci.prebuilt.shajra = joinForCi "prebuilt-shajra" isAny build.pkgs.shajra.prebuilt;
  ci.build.nixpkgs.split1 = joinForCi "build-nixpkgs" isSplit1 build.pkgs.nixpkgs.build;
  ci.build.nixpkgs.split2 = joinForCi "build-nixpkgs" isSplit2 build.pkgs.nixpkgs.build;
  ci.build.nixpkgs.split3 = joinForCi "build-nixpkgs" isSplit3 build.pkgs.nixpkgs.build;
  ci.build.nixpkgs.split4 = joinForCi "build-nixpkgs" isSplit4 build.pkgs.nixpkgs.build;
  ci.build.haskell-nix = joinForCi "build-haskellnix" isAny build.pkgs.haskell-nix.build;
  ci.build.shajra = joinForCi "build-shajra" isAny build.pkgs.shajra.build;
  ci.all = joinForCi "all" isAny build.pkgs;
  ci.check-prebuilt = checkPrebuilt;
  ci.check-build = checkBuild;

in
{
  shajra-provision = {
    infra = { inherit (build) np hn; };
    inherit config pkgs ci;
  };
}
