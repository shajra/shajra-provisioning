{ build, ... }:

{
    imports = [
        ../../../ubiquity
        ../../tui/all
    ];

    home.extraPackages = build.pkgs.lists.chat.gui.all;
}
