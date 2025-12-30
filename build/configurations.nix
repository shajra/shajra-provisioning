inputs: withSystem:

# DESIGN: This library only works when the build's `shajra-provision` attribute
# has been set as the flake's `legacyPackages`.

{

  nixosConfiguration =
    {
      system,
      path',
      privateModules ? [ ],
    }:
    withSystem system (
      { config, ... }:
      config.legacyPackages.infra.np.nixpkgs.system.nixos {
        _module.args.build = config.legacyPackages;
        imports = [ path' ] ++ privateModules;
      }
    );

  darwinConfiguration =
    {
      system,
      path',
      privateModules ? [ ],
    }:
    withSystem system (
      { config, ... }:
      inputs.nix-darwin.lib.darwinSystem {
        pkgs = config.legacyPackages.infra.np.nixpkgs.system;
        specialArgs.build = config.legacyPackages;
        modules = [ path' ] ++ privateModules;
      }
    );

  homeConfiguration =
    {
      system,
      path',
      privateModules ? [ ],
    }:
    withSystem system (
      { config, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = config.legacyPackages.infra.np.nixpkgs.home;
        extraSpecialArgs.build = config.legacyPackages;
        modules = [ path' ] ++ privateModules;
      }
    );

}
