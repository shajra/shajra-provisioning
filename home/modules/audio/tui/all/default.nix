{ build, ... }:

{
    imports = [ ../../../ubiquity ];
    home.extraPackages = build.pkgs.lists.audio.tui.all;
    programs.ncmpcpp.enable = true;
}
