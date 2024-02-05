{ config, lib, pkgs, build, ... }:

{
    imports = [
        build.infra.np.nixpkgs.home.homeModules.vscode-server
        ../../../ubiquity
        ../all
        ../../tui/linux
    ];

    gtk = import ./gtk config;

    home.file = import home/file config pkgs;
    home.extraPackages = build.pkgs.lists.base.gui.linux;

    programs.bash = import programs/bash;
    programs.firefox = import programs/firefox config pkgs;
    programs.fish = import programs/fish config pkgs;
    programs.i3status-rust = import programs/i3status-rust config pkgs;
    programs.qutebrowser.enable = true;
    programs.rofi = import programs/rofi config pkgs;
    programs.zathura.enable = true;

    services.clipmenu.enable = true;
    services.dunst = import services/dunst config pkgs;
    services.flameshot = import services/flameshot config;
    services.gammastep.enable = true;
    services.gammastep.provider = "geoclue2";
    services.gromit-mpx.enable = false;  # DESIGN: just gets in the way mostly
    services.vscode-server.enable = true;
    services.xsuspender = import services/xsuspender;

    xresources = import ./xresources config pkgs;

    xsession = import ./xsession config pkgs lib;
}
