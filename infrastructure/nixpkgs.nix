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

    nixos = import sources."nixos-stable" {
        config = config.nixpkgs;
        overlays = [overlay];
    };

    nixpkgs = import sources.nixpkgs-unstable {
        config = config.nixpkgs;
        overlays = [overlay];
    };

    pickPkgs = pkgs: if pkgs == "stable" then nixos else nixpkgs;

    v = config.hackage.version;

    # DESIGN: These Nixpkgs builds aren't used, but keeping around in case
    # useful later.
    hsPkgs.ghc865 = hs: hs.packages.ghc865.override {
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

    hsPkgs.ghc883 = hs:
        let laxBuild = drv:
            let unbrokenDrv = drv.overrideAttrs (old: {
                broken = false;
                meta = {};
            });
            in hs.lib.doJailbreak
                (hs.lib.dontCheck unbrokenDrv);
        in hs.packages.ghc883.override {
            overrides = hSelf: hSuper: {
                haskell-ci = laxBuild hSuper.haskell-ci;
                cabal-install-parsers =
                    laxBuild hSuper.cabal-install-parsers;
            };
        };

    hsPkgs.ghc8101 = hs: hs.packages.ghc8101;

in rec {

    inherit nixos nixpkgs;

    hs.fromTopLevel = nixpkgsName: hsPkgName:
        let pkgs = pickPkgs nixpkgsName;
        in {
            ${hsPkgName} = pkgs.haskell.lib.justStaticExecutables
                pkgs."${hsPkgName}";
        };

    hs.fromPackages = nixpkgsName: ghcVersion: hsPkgName:
        let hs = (pickPkgs nixpkgsName).haskell;
        in {
            ${hsPkgName} = hs.lib.justStaticExecutables
                (hsPkgs."${ghcVersion}" hs)."${hsPkgName}";
        };

}
