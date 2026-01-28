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

  home.activation = import home/activation config lib pkgs;

  xdg.configFile = import xdg/configFile;
}
