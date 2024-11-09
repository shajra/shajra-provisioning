{ infraConfig
, inputs
, isDarwin
, system
}:

let

    nixpkgs =
        let hn = inputs.haskell-nix;
            nixpkgsSrc = hn.inputs."${infraConfig.haskell-nix.nixpkgs-pin}";
            nixpkgsArgs = {
                inherit system;
                inherit (hn) config;
                overlays = [ hn.overlays.combined ];
            };
        in import nixpkgsSrc nixpkgsArgs;

    inherit (nixpkgs) haskell-nix;

    allExes = pkg: pkg.components.exes;

    planConfigFor = ghcVersion: name: custom:
        let
            index-state = infraConfig.haskell-nix.hackage.index.state or null;
            index-sha256 = infraConfig.haskell-nix.hackage.index.sha256 or null;
            modules = custom.modules or defaultModules;
            configureArgs = custom.configureArgs or null;
        in {
            inherit name modules;
            compiler-nix-name = ghcVersion;
            ${if (configureArgs == null) then null else "configureArgs"} = configureArgs;
            ${if (index-state == null) then null else "index-state"} = index-state;
            ${if (index-sha256 == null) then null else "index-sha256"} = index-sha256;
            sha256map = infraConfig.haskell-nix.sha256map or {};
        };

    hackagePlanConfigFor = ghcVersion: name: custom:
        planConfigFor ghcVersion name custom // {
            version = infraConfig.hackage.version."${name}";
        };

    sourcePlanConfigFor = ghcVersion: name: custom:
        planConfigFor ghcVersion name custom // {
            src = inputs."${name}";
        };

    defaultModules = [{ enableSeparateDataOutput = true; }];

    defaultReinstallableLibGhcModules = defaultModules ++ [{
        reinstallableLibGhc = true;
    }];

in rec {

    inherit haskell-nix nixpkgs defaultModules defaultReinstallableLibGhcModules;

    fromHackageCustomized = ghcVersion: name: custom:
        let planConfig = hackagePlanConfigFor ghcVersion name custom;
        in allExes (haskell-nix.hackage-package planConfig);

    fromHackageReinstallableLibGhc = ghcVersion: name:
        fromHackageCustomized ghcVersion name {
            modules = defaultReinstallableLibGhcModules;
        };

    fromHackage = ghcVersion: name:
        fromHackageCustomized ghcVersion name {};

    hackageUpdateMaterializedCustomized = ghcVersion: name: custom:
        let planConfig = hackagePlanConfigFor ghcVersion name custom;
            inherit ((haskell-nix.hackage-project planConfig)) plan-nix;
        in {
            "${name}-updateMaterialized" = plan-nix.passthru.updateMaterialized;
        };

    hackageUpdateMaterialized = ghcVersion: name:
        hackageUpdateMaterializedCustomized ghcVersion name {};


    fromSourceCustomized = ghcVersion: name: custom:
        let planConfig = sourcePlanConfigFor ghcVersion name custom;
        in allExes (haskell-nix.cabalProject planConfig)."${name}";

    fromSourceReinstallableLibGhc = ghcVersion: name:
        fromSourceCustomized ghcVersion name {
            modules = defaultReinstallableLibGhcModules;
        };

    fromSource = ghcVersion: name:
        fromSourceCustomized ghcVersion name {};

    sourceUpdateMaterializedCustomized = ghcVersion: name: custom:
        let planConfig = sourcePlanConfigFor ghcVersion name custom;
            inherit ((haskell-nix.cabalProject' planConfig)) plan-nix;
        in {
            "${name}-updateMaterialized" = plan-nix.passthru.updateMaterialized;
        };

    sourceUpdateMaterialized = ghcVersion: name:
        sourceUpdateMaterializedCustomized ghcVersion name {};

}
