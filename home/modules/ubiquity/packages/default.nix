{ config, lib, ... }:

{
  config.home.packages = config.home.extraPackages;
  options = {
    home.extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Extra packages beyond those included by Home Manager.  This
        allows these packages to be overridden, or removed for a slimmer
        build.
      '';
    };
  };
}
