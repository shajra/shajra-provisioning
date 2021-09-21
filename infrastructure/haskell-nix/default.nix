{ checkMaterialization
, infraConfig
, sources
, isDarwin
}:

let

    nixpkgs =
        let hn = import sources."haskell.nix" {};
            nixpkgsSrc = hn.sources."${infraConfig.haskell-nix.nixpkgs-pin}";
            nixpkgsOrigArgs = hn.nixpkgsArgs;
            nixpkgsArgs = nixpkgsOrigArgs // {
                config = {};
                overlays = nixpkgsOrigArgs.overlays ++ [(self: super: {
                    alex = super.haskellPackages.alex;
                    happy = super.haskellPackages.happy;
                })];
            };
        in import nixpkgsSrc nixpkgsArgs;

    haskell-nix = nixpkgs.haskell-nix;

    allExes = pkg: pkg.components.exes;

    planConfigFor = ghcVersion: name: modules:
        let materializedBase =
                if builtins.elem name infraConfig.haskell-nix.platformSensitive
                then
                    if isDarwin
                    then ./materialized-darwin
                    else ./materialized-linux
                else ./materialized-common;
            materialized = materializedBase + "/${name}";
            index-state = infraConfig.haskell-nix.hackage.index.state or null;
            index-sha256 = infraConfig.haskell-nix.hackage.index.sha256 or null;
        in {
            inherit name modules materialized checkMaterialization;
            compiler-nix-name = ghcVersion;
            ${if isNull index-state then null else "index-state"} = index-state;
            ${if isNull index-sha256 then null else "index-sha256"} = index-sha256;
            lookupSha256 = {location, ...}:
                infraConfig.haskell-nix.lookupSha256."${location}" or null;
        };

    hackagePlanConfigFor = ghcVersion: name: modules:
        planConfigFor ghcVersion name modules // {
            version = infraConfig.hackage.version."${name}";
        };

    sourcePlanConfigFor = ghcVersion: name: modules:
        planConfigFor ghcVersion name modules // {
            src = sources."${name}";
        };

    defaultModules = [{ enableSeparateDataOutput = true; }];

    defaultReinstallableLibGhcModules = defaultModules ++ [{
        reinstallableLibGhc = true;
    }];

in rec {

    inherit haskell-nix nixpkgs;

    fromHackageWithModules = ghcVersion: name: modules:
        let planConfig = hackagePlanConfigFor ghcVersion name modules;
        in allExes (haskell-nix.hackage-package planConfig);

    fromHackageReinstallableLibGhc = ghcVersion: name:
        fromHackageWithModules ghcVersion name
            defaultReinstallableLibGhcModules;

    fromHackage = ghcVersion: name:
        fromHackageWithModules ghcVersion name defaultModules;

    hackageUpdateMaterializedWithModules = ghcVersion: name: modules:
        let planConfig = hackagePlanConfigFor ghcVersion name modules;
            plan-nix = (haskell-nix.hackage-project planConfig).plan-nix;
        in {
            "${name}" = plan-nix.passthru.updateMaterialized;
        };

    hackageUpdateMaterialized = ghcVersion: name:
        hackageUpdateMaterializedWithModules ghcVersion name defaultModules;


    fromSourceWithModules = ghcVersion: name: modules:
        let planConfig = sourcePlanConfigFor ghcVersion name modules;
        in allExes (haskell-nix.cabalProject planConfig)."${name}";

    fromSourceReinstallableLibGhc = ghcVersion: name:
        fromSourceWithModules ghcVersion name
            defaultReinstallableLibGhcModules;

    fromSource = ghcVersion: name:
        fromSourceWithModules ghcVersion name defaultModules;

    sourceUpdateMaterializedWithModules = ghcVersion: name: modules:
        let planConfig = sourcePlanConfigFor ghcVersion name modules;
            plan-nix = (haskell-nix.cabalProject' planConfig).plan-nix;
        in {
            "${name}" = plan-nix.passthru.updateMaterialized;
        };

    sourceUpdateMaterialized = ghcVersion: name:
        sourceUpdateMaterializedWithModules ghcVersion name defaultModules;

}
