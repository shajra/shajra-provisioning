{ build, config, ... }:

{
    imports = [ ../../ubiquity ];
    home.file = import home/file config.home.homeDirectory;
    home.extraPackages = build.pkgs.lists.programming.db;
}
