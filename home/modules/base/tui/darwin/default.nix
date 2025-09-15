{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../../../ubiquity
    ../all
  ];

  disabledModules = [ "targets/darwin/linkapps.nix" ];
  home.activation = import home/activation config lib pkgs;

  xdg.configFile = import xdg/configFile;
}
