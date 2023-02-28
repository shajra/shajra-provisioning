{ build, ... }:

{
    imports = [ ../../ubiquity ];
    home.extraPackages = build.pkgs.lists.documentation.all;
}
