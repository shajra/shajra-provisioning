{ build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
    ];

    home.extraPackages = build.pkgs.lists.audio.tui.linux;
}
