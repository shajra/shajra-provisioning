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

    programs.bash = import programs/bash;
    programs.firefox = import programs/firefox config pkgs;
    programs.fish = import programs/fish config pkgs;
    programs.i3status-rust = import programs/i3status-rust config pkgs;
    # REVISIT: BROKEN: 2025-06-22
    #programs.qutebrowser = import programs/qutebrowser lib;
    programs.rofi = import programs/rofi config pkgs;
    programs.vscode = import programs/vscode;

    services.clipmenu.enable = true;
    services.dunst = import services/dunst config pkgs;
    services.flameshot = import services/flameshot config;
    services.gammastep.enable = true;
    services.gammastep.provider = "geoclue2";
    services.gromit-mpx.enable = false;  # DESIGN: just gets in the way mostly
    services.vscode-server.enable = true;

    xresources = import ./xresources config pkgs;

    xsession = import ./xsession config pkgs lib;
}
