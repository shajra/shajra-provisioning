{ build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
        ../../tui/linux
    ];
    home.extraPackages = build.pkgs.lists.chat.gui.linux;
}
