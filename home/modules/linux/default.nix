{ config, pkgs, ... }:

let
    build = import ../../.. {};
    sources = build.sources;
in

{
    imports = [ ../base ];

    gtk = import ./gtk pkgs;

    home.file = import home/file config pkgs sources;

    programs.bash = import programs/bash;
    programs.firefox = import programs/firefox config pkgs;
    programs.fish = import programs/fish config pkgs;
    programs.i3status-rust.enable = true;
    programs.rofi = import programs/rofi;
    programs.texlive = import programs/texlive;
    programs.zathura.enable = true;

    services.clipmenu.enable = true;
    services.dunst = import services/dunst config pkgs;
    services.emacs = import services/emacs;
    services.gammastep.enable = true;
    services.gammastep.provider = "geoclue2";
    services.picom.enable = true;
    services.xsuspender = import services/xsuspender;

    xdg.mimeApps = import xdg/mimeApps;

    xresources = import ./xresources config;

    xsession = import ./xsession config pkgs;
}
