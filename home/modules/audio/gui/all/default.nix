{ build, ... }:

{
    imports = [
        ../../../ubiquity
        ../../tui/all
    ];
    home.extraPackages = build.pkgs.lists.audio.gui.all;
}
