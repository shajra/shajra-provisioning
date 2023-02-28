{ build, ... }:

{
    imports = [ ../../ubiquity ];
    home.extraPackages = build.pkgs.lists.os.nixos;
}
