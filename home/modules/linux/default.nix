{ config, pkgs, ... }:

let
    build = import ../../.. {};
    sources = build.sources;
in

{
    imports = [ ../base ];

    home.file = import home/file sources;

    programs.bash = import programs/bash;
    programs.firefox = import programs/firefox pkgs;
    programs.i3status-rust.enable = true;
    programs.rofi.enable = true;
    programs.zathura.enable = true;

    services.gammastep.enable = true;
    services.gammastep.provider = "geoclue2";

    xresources = import ./xresources;

    xsession.enable = true;
    xsession.windowManager.i3 = import xsession/windowManager/i3 config pkgs;
}
