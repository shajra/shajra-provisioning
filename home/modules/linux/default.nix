{ config, pkgs, ... }:

let
    build = import ../../.. {};
    sources = build.sources;
in

{
    imports = [ ../base ];

    home.file = import home/file config pkgs sources;

    programs.bash = import programs/bash;
    programs.firefox = import programs/firefox pkgs;
    programs.i3status-rust.enable = true;
    programs.rofi = import programs/rofi;
    programs.zathura.enable = true;

    services.dunst = import services/dunst config pkgs;
    services.gammastep.enable = true;
    services.gammastep.provider = "geoclue2";
    services.picom.enable = true;
    services.xsuspender = import services/xsuspender;

    xresources = import ./xresources;

    xsession = import ./xsession config pkgs;
}
