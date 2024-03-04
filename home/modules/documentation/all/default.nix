{ build, ... }:

{
    imports = [ ../../ubiquity ];
    home.extraPackages = build.pkgs.lists.documentation.all;
    programs.pandoc.enable = true;
}
