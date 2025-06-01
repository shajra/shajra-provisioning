{ build, ... }:

{
    imports = [
        ../../ubiquity
        ../tui
    ];

    home.extraPackages = build.pkgs.lists.chat.gui;
}
