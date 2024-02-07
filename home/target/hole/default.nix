{ config, pkgs, build, ... }:

let
    userConfig = build.config.provision.user;
in

{
    imports = [
        ../../modules/base/gui/linux

        ../../modules/audio/gui/all
        ../../modules/audio/tui/all
        ../../modules/audio/tui/linux

        ../../modules/chat/gui/all
        ../../modules/chat/gui/linux
        ../../modules/chat/tui/all

        ../../modules/documentation/all
        ../../modules/documentation/linux

        ../../modules/finance

        ../../modules/os/nixos

        ../../modules/peripheral/wifi/gui/linux
        ../../modules/peripheral/wifi/tui/linux

        ../../modules/programming/c/all
        ../../modules/programming/c/linux
        ../../modules/programming/db
        ../../modules/programming/general
        ../../modules/programming/haskell
        ../../modules/programming/java
        ../../modules/programming/python
        ../../modules/programming/scala
        ../../modules/programming/shell

        ../../modules/sync
    ];

    home.file = import home/file config pkgs userConfig;
    home.homeDirectory = userConfig.hole.homeDirectory;
    home.username = userConfig.hole.username;
    programs.alacritty.settings.font.size = 11.0;
    programs.autorandr = import programs/autorandr pkgs.i3-dpi;
    programs.i3status-rust = import programs/i3status-rust config pkgs;
    programs.kitty.extraConfig = "font_size 11";
}
