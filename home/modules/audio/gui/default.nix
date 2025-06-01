{ build, ... }:

{
    imports = [
        ../../ubiquity
        ../tui
    ];
    home.extraPackages = build.pkgs.lists.audio.gui;
}
