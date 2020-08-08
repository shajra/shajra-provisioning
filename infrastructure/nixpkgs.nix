{ config
, sources
}:

let

    overlay = self: super: {

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

    mkNixpkgs = s: import s {
        config = config.nixpkgs;
        overlays = [overlay];
    };

    nixpkgs-stable   = mkNixpkgs sources.nixpkgs-stable;
    nixpkgs-unstable = mkNixpkgs sources.nixpkgs-unstable;

    lib = nixpkgs-stable.lib;

    pickPkgs = pkgs:
        if pkgs == "stable"
        then nixpkgs-stable
        else nixpkgs-unstable;

    v = config.hackage.version;

    # DESIGN: These Nixpkgs builds aren't used, but keeping around in case
    # useful later.
    hsOverrides.ghc865 = hs: hs.packages.ghc865.override {
        overrides = hSelf: hSuper: {

            # IDEA: are doJailbreaks/dontChecks still needed?
            # DESIGN: not updating libraries globally to minimize cache misses

            ghcide = hs.lib.dontCheck (
                hSuper.callCabal2nix "ghcide" sources.ghcide {
                    haskell-lsp = hs.lib.dontCheck (
                        hSuper.callHackage "haskell-lsp" v.haskell-lsp {
                            haskell-lsp-types = hs.lib.dontCheck (
                                hSuper.callHackage "haskell-lsp-types" v.haskell-lsp-types {}
                            );
                        }
                    );
                    hie-bios = hs.lib.dontCheck (
                        hSuper.callCabal2nix "hie-bios" sources.hie-bios {}
                    );
                 }
            );

            hie-bios = hs.lib.dontCheck (
                hSuper.callCabal2nix "hie-bios" sources.hie-bios {}
            );
        };
    };

    # DESIGN: These Nixpkgs builds aren't used, but keeping around in case
    # useful later.
    hsOverrides.ghc883 = hs:
        let laxBuild = drv:
            let unbrokenDrv = drv.overrideAttrs (old: {
                broken = false;
                meta = {};
            });
            in hs.lib.doJailbreak
                (hs.lib.dontCheck unbrokenDrv);
        in hs.packages.ghc883.override {
            overrides = hSelf: hSuper: {
                cabal-install-parsers =
                    laxBuild hSuper.cabal-install-parsers;
            };
        };

in {

    inherit nixpkgs-stable nixpkgs-unstable;

    pick = nixpkgsName: paths:
        let pkgs = pickPkgs nixpkgsName;
            pick' = p:
                let path = lib.splitString "." p;
	            attrName = lib.concatStrings (lib.intersperse "-" path);
                    pkg = lib.getAttrFromPath path pkgs;
                in { "${attrName}" = pkg; };
        in lib.fold (a: b: a // b) {} (map pick' paths);

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
