{ infraConfig
, inputs
, inputs'
, isDarwin
, lib
, nixpkgs
, pkgsConfig
, system
}:

let

    inherit (infraConfig.nixpkgs) config masterPkgsOverUnstable;

    overlays =  import ./overlays.nix { inherit inputs inputs'; };

    remasterOverlay = name:
        if name == "unstable"
        then self: super: masterPkgsOverUnstable nixpkgsOverlaid.master
        else self: super: {};

    mkNixpkgs = name: pkgs: import pkgs.path {
        inherit config system;
        overlays = [(remasterOverlay name)] ++ overlays;
    };

    nixpkgsOverlaid =
        let pkgs = builtins.mapAttrs mkNixpkgs nixpkgs;
        in pkgs // {
            home =
                if isDarwin
                then pkgs."${pkgsConfig.home.darwin}"
                else pkgs."${pkgsConfig.home.linux}";
            system =
                if isDarwin
                then pkgs."${pkgsConfig.system.darwin}"
                else pkgs."${pkgsConfig.system.linux}";
        };

    nixpkgsRemastered = nixpkgsOverlaid;

    pickPkgs = name: nixpkgsRemastered."${name}";

    # DESIGN: not used any more, but maybe later
    hsOverrides.ghc865 = hs: hs.packages.ghc865.override {
        overrides = hSelf: hSuper: {
        };
    };

in {

    inherit config overlays;

    nixpkgs = nixpkgsRemastered;

    pick = {linux ? null, darwin ? null}: paths:
        let pkgs =
                if (isDarwin && ! builtins.isNull darwin)
                then pickPkgs darwin
                else if (! isDarwin && ! builtins.isNull linux)
                then pickPkgs linux
                else {};
            pick' = p:
                let path = lib.splitString "." p;
                    attrName = lib.concatStrings (lib.intersperse "-" path);
                    pkg = lib.getAttrFromPath path pkgs;
                in { "${attrName}" = pkg; };
            paths' =
                if (isDarwin && ! builtins.isNull darwin)
                    || (! isDarwin && ! builtins.isNull linux)
                then paths
                else [];
        in lib.fold (a: b: a // b) {} (map pick' paths');

    hs.fromTopLevel = nixpkgsName: hsPkgName:
        let pkgs = pickPkgs nixpkgsName;
        in {
            ${hsPkgName} = pkgs.haskell.lib.justStaticExecutables
                pkgs."${hsPkgName}";
        };

    hs.fromPackages = nixpkgsName: ghcVersion: hsPkgName:
        let hs = (pickPkgs nixpkgsName).haskell;
            hsOverridesDefault = hs: hs.packages.${ghcVersion};
            hsPkgs = (hsOverrides."${ghcVersion}" or hsOverridesDefault) hs;
        in {
            ${hsPkgName} =
                hs.lib.justStaticExecutables hsPkgs."${hsPkgName}";
        };

}
