{ config, pkgs, ... }:

{
    imports = [ ../../modules/linux ];

    home.homeDirectory = "/home/tnks";
    home.username = "tnks";

    home.file = import home/file config pkgs;

    programs.autorandr = import programs/autorandr pkgs.i3-init;
    programs.i3status-rust = import programs/i3status-rust config pkgs;
}
