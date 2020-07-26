{ checkMaterialization
, useMaterialization
, config
, sources
}:

let
    haskell-nix =
        let hn = import sources."haskell.nix" {};
            nixpkgsSrc = hn.sources."${config.haskell-nix.nixpkgs-pin}";
            nixpkgsOrigArgs = hn.nixpkgsArgs;
            nixpkgsArgs = nixpkgsOrigArgs // {
                config = {};
                overlays = nixpkgsOrigArgs.overlays ++ [(self: super: {
                    alex = super.haskellPackages.alex;
                    happy = super.haskellPackages.happy;
                })];
            };
        in (import nixpkgsSrc nixpkgsArgs).haskell-nix;

    allExes = pkg: pkg.components.exes;

    planConfigFor = ghcVersion: name: modules:
        let plan-sha256 = config.haskell-nix.plan."${name}".sha256 or null;
            materialized = haskell-nix/materialized + "/${name}";
            isMaterialized = builtins.pathExists materialized
                && useMaterialization;
            check = config.haskell-nix.plan."${name}".check
                or checkMaterialization;
        in {
            inherit name modules;
            compiler-nix-name = ghcVersion;
            index-state = config.haskell-nix.hackage.index.state;
            index-sha256 = config.haskell-nix.hackage.index.sha256;
            ${if plan-sha256 != null then "plan-sha256" else null} =
                plan-sha256;
            ${if isMaterialized then "materialized" else null} =
                materialized;
            ${if isMaterialized then "checkMaterialization" else null} =
                check;
        };

    defaultModules = [{ enableSeparateDataOutput = true; }];

in rec {

    inherit haskell-nix;

    fromHackageWithModules = ghcVersion: name: modules:
        let planConfig = planConfigFor ghcVersion name modules // {
                version = config.hackage.version."${name}";
            };
        in allExes (haskell-nix.hackage-package planConfig);

    fromHackage = ghcVersion: name:
        fromHackageWithModules ghcVersion name defaultModules;

    fromHackageReinstallableLibGhc = ghcVersion: name:
        fromHackageWithModules ghcVersion name (defaultModules ++ [{
            reinstallableLibGhc = true;
        }]);

    fromStackage = name: allExes
        haskell-nix.snapshots."${config.stackage.resolver}"."${name}";

    fromSourceWithModules = ghcVersion: name: modules:
        let planConfig = planConfigFor ghcVersion name modules // {
                src = sources."${name}";
            };
        in allExes (haskell-nix.cabalProject planConfig)."${name}";

    fromSource = ghcVersion: name:
        fromSourceWithModules ghcVersion name defaultModules;

    fromSourceReinstallableLibGhc = ghcVersion: name:
        fromSourceWithModules ghcVersion name (defaultModules ++ [{
            reinstallableLibGhc = true;
        }]);

}
