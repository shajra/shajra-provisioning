{ infraConfig
, sources
, isDarwin
}:

let

    # DESIGN: downloading the latest hashes is time-consuming
    #overlay.all-cabal-hashes = self: super: {
    #    all-cabal-hashes = sources.all-cabal-hashes;
    #};

    overlay.emacs = import sources.emacs-overlay;
    overlay.nix-project = self: super: import sources.nix-project;
    overlay.upgrades = self: super: {
        hasklig = super.hasklig.overrideAttrs (old: {
            url = sources.hasklig.url;
            sha256 = sources.hasklig.sha256;
        });
        home-manager = super.home-manager.overrideAttrs (old: {
            src = sources.home-manager;
        });
        skhd = super.skhd.overrideAttrs (old: { src = sources.skhd; });
        yabai = super.yabai.overrideAttrs (old: { src = sources.yabai; });
    };

    overlay.provided = import ./overlay;

    overlays = with overlay; [
        emacs
        nix-project
        upgrades
        provided
    ];

    config = infraConfig.nixpkgs // {
        packageOverrides = pkgs: {
            nur = import sources.nur { inherit pkgs; };
        };
    };

    mkNixpkgs = s: import s { inherit config overlays; };

    nixpkgs-stable   = mkNixpkgs sources.nixpkgs-stable;
    nixpkgs-unstable = mkNixpkgs sources.nixpkgs-unstable;
    nixpkgs-home = mkNixpkgs sources.nixpkgs-home;
    nixpkgs-system = mkNixpkgs sources.nixpkgs-system;

    lib = nixpkgs-stable.lib;

    pickPkgs = pkgs:
        if pkgs == "stable"
        then nixpkgs-stable
        else nixpkgs-unstable;

    v = infraConfig.hackage.version;

    # DESIGN: not used any more, but maybe later
    hsOverrides.ghc865 = hs: hs.packages.ghc865.override {
        overrides = hSelf: hSuper: {
        };
    };

in {

    inherit nixpkgs-stable nixpkgs-unstable nixpkgs-home nixpkgs-system;
    inherit config overlays;

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
