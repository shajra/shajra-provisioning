{ config
, sources
, isDarwin
}:

let

    overlay.provided = import ./overlay;

    # DESIGN: downloading the latest hashes is time-consuming
    #overlay.all-cabal-hashes = self: super: {
    #    all-cabal-hashes = sources.all-cabal-hashes;
    #};

    overlay.emacs = import sources.emacs-overlay;
    overlay.nix-project = self: super: import sources.nix-project;

    overlays = with overlay; [
        emacs
        nix-project
        provided
    ];

    mkNixpkgs = s: import s {
        config = config.nixpkgs;
        inherit overlays;
    };

    nixpkgs-stable   = mkNixpkgs sources.nixpkgs-stable;
    nixpkgs-unstable = mkNixpkgs sources.nixpkgs-unstable;

    lib = nixpkgs-stable.lib;

    pickPkgs = pkgs:
        if pkgs == "stable"
        then nixpkgs-stable
        else nixpkgs-unstable;

    v = config.hackage.version;

    # DESIGN: not used any more, but maybe later
    hsOverrides.ghc865 = hs: hs.packages.ghc865.override {
        overrides = hSelf: hSuper: {
        };
    };

in {

    inherit nixpkgs-stable nixpkgs-unstable overlays;

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
