{ config
, sources
, isDarwin
}:

let

    overlay.global = self: super: {

        # DESIGN: downloading the latest hashes is time-consuming
        #all-cabal-hashes = sources.all-cabal-hashes;

        global = super.global.overrideAttrs (oldAttrs: {
            postInstall = ''
              mkdir -p "$out/share/emacs/site-lisp"
              cp -v *.el "$out/share/emacs/site-lisp"

              wrapProgram $out/bin/gtags \
                --set GTAGSCONF "$out/share/gtags/gtags.conf" \
                --prefix PYTHONPATH : "$(toPythonPath ${super.pythonPackages.pygments})"
              wrapProgram $out/bin/global \
                --set GTAGSCONF "$out/share/gtags/gtags.conf" \
                --prefix PYTHONPATH : "$(toPythonPath ${super.pythonPackages.pygments})"
            '';
        });

    };

    # DESIGN: Doom Emacs isn't ready yet for GCC Emacs
    overlay.emacs = import sources.emacs-overlay;

    mkNixpkgs = s: import s {
        config = config.nixpkgs;
        overlays = with overlay; [global emacs];
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

    inherit nixpkgs-stable nixpkgs-unstable;

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
