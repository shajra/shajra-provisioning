{ build, ... }:

{
    imports = [ ../../../ubiquity ];
    home.extraPackages = build.pkgs.lists.audio.tui.linux;
}
