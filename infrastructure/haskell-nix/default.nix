{ checkMaterialization
, config
, sources
, isDarwin
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
        let materializedBase =
                if builtins.elem name config.haskell-nix.platformSensitive
                then
                    if isDarwin
                    then ./materialized-darwin
                    else ./materialized-linux
                else ./materialized-common;
            materialized = materializedBase + "/${name}";
            check = config.haskell-nix.plan."${name}".check
                or checkMaterialization;
        in {
            inherit name modules materialized checkMaterialization;
            compiler-nix-name = ghcVersion;
            index-state = config.haskell-nix.hackage.index.state;
            index-sha256 = config.haskell-nix.hackage.index.sha256;
            lookupSha256 = {location, ...}:
                config.haskell-nix.lookupSha256."${location}" or null;
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
