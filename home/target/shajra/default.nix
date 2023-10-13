{ config, pkgs, lib, build, ... }:

let
    userConfig = build.config.provision.user;
    hostname = "shajra";
in

{
    imports = [
        ../../modules/base/gui/linux
        ../../modules/programming/db
    ];

    home.file = import home/file config;
    home.homeDirectory = userConfig."${hostname}".homeDirectory;
    home.username = userConfig."${hostname}".username;

    programs.git = import programs/git lib;
    programs.i3status-rust = import programs/i3status-rust config pkgs;

    services.gammastep.enable = lib.mkForce false;

    targets.genericLinux.enable = true;

    xdg.configFile = import xdg/configFile config;

    xsession.windowManager.i3 = import xsession/windowManager/i3 config lib;
}
