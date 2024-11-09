inputs: withSystem:
final: prev:

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
    lowerBase32Chars  = builtins.substring  0 11 base32Chars;
    middleBase32Chars = builtins.substring 11 11 base32Chars;
    upperBase32Chars  = builtins.substring 22 10 base32Chars;
    isInSection = section: drv:
        let hashChar = builtins.substring 11 1 drv.outPath;
        in lib.elem hashChar (lib.stringToCharacters section);
    isLower  = isInSection lowerBase32Chars;
    isMiddle = isInSection middleBase32Chars;
    isUpper  = isInSection upperBase32Chars;
    isAny = drv: true;

    isDrvSet = s:
        lib.isAttrs s && (lib.any lib.isDerivation (builtins.attrValues s) || s == {});

    joinForCi = name: filter: set:
        let found = deepMerge (lib.collect isDrvSet set);
            filtered = lib.filterAttrs (_n: filter) found;
        in prev.linkFarm "shajra-provision-ci-${name}" filtered;

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
            inherit inputs' isDarwin isDevBuild np hn;
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

    checkCaching = sets: checkCacheHit:
        let
            found = builtins.attrValues (deepMerge (lib.collect isDrvSet sets));
            bashOutPaths = lib.concatMapStringsSep " " (d: d.outPath) found;
        in prev.writeShellApplication {
            name = "check-prebuilt";
            runtimeInputs = [
                prev.coreutils
                prev.curl
            ];
            text = ''
                set -eu
                set -o pipefail
                OUT_PATHS=(${bashOutPaths})
                EXIT_CODE=0
                echo "${
                    if checkCacheHit
                    then "Checking prebuilt packages cached..."
                    else "Checking packages to build uncached..."
                }"
                for path in "''${OUT_PATHS[@]}"
                do
                    PATH_PREFIX="''${path%%-*}"
                    HASH="$(basename "$PATH_PREFIX")"
                    if ${ if checkCacheHit then "!" else "" } curl --fail --silent --head \
                        "https://cache.nixos.org/$HASH.narinfo" >/dev/null
                    then
                        echo "${ if checkCacheHit then "NOT" else "" } FOUND: $path"
                        EXIT_CODE=1
                    fi
                done
                exit "$EXIT_CODE"
            '';
        };

    checkPrebuilt =
        let sets = build.pkgs.nixpkgs.prebuilt
                // build.pkgs.haskell-nix.prebuilt
                // build.pkgs.shajra.prebuilt;
        in checkCaching sets true;

    checkBuild =
        let sets = build.pkgs.nixpkgs.build
                // build.pkgs.haskell-nix.build
                // build.pkgs.shajra.build;
        in checkCaching sets false;

    ci.prebuilt.nixpkgs.lower  = joinForCi "prebuilt-nixpkgs"    isLower  build.pkgs.nixpkgs.prebuilt;
    ci.prebuilt.nixpkgs.middle = joinForCi "prebuilt-nixpkgs"    isMiddle build.pkgs.nixpkgs.prebuilt;
    ci.prebuilt.nixpkgs.upper  = joinForCi "prebuilt-nixpkgs"    isUpper  build.pkgs.nixpkgs.prebuilt;
    ci.prebuilt.haskell-nix    = joinForCi "prebuilt-haskellnix" isAny    build.pkgs.haskell-nix.prebuilt;
    ci.prebuilt.shajra         = joinForCi "prebuilt-shajra"     isAny    build.pkgs.shajra.prebuilt;
    ci.build.nixpkgs.lower     = joinForCi "build-nixpkgs"       isLower  build.pkgs.nixpkgs.build;
    ci.build.nixpkgs.middle    = joinForCi "build-nixpkgs"       isMiddle build.pkgs.nixpkgs.build;
    ci.build.nixpkgs.upper     = joinForCi "build-nixpkgs"       isUpper  build.pkgs.nixpkgs.build;
    ci.build.haskell-nix       = joinForCi "build-haskellnix"    isAny    build.pkgs.haskell-nix.build;
    ci.build.shajra            = joinForCi "build-shajra"        isAny    build.pkgs.shajra.build;
    ci.all                     = joinForCi "all"                 isAny    build.pkgs;
    ci.check-prebuilt = checkPrebuilt;
    ci.check-build = checkBuild;

in {
    shajra-provision = {
        infra = { inherit (build) np hn; };
        inherit config pkgs ci;
    };
}
