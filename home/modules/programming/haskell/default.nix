{ build, config, ... }:

{
    imports = [ ../../ubiquity ];
    home.file = import home/file;
    home.extraPackages = build.pkgs.lists.programming.haskell;
    xdg.configFile = import xdg/configFile;
}
