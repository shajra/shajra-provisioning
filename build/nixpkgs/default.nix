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

    config = infraConfig.nixpkgs;

    overlays = import ./overlays { inherit inputs inputs'; };

    mkNixpkgs = pkgs: import pkgs.path { inherit config overlays system; };

    nixpkgs' =
        let pkgs = builtins.mapAttrs (name: mkNixpkgs) nixpkgs;
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

    pickPkgs = name: nixpkgs'."${name}";

    v = infraConfig.hackage.version;

    # DESIGN: not used any more, but maybe later
    hsOverrides.ghc865 = hs: hs.packages.ghc865.override {
        overrides = hSelf: hSuper: {
        };
    };

in {

    inherit config overlays;

    nixpkgs = nixpkgs';

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
