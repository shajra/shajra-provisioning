{ config, pkgs, build, ... }:

{
    imports = [
        ../../../ubiquity
        ../../tui/all
    ];

    fonts.fontconfig.enable = true;

    home.extraPackages = build.pkgs.lists.base.gui.all;

    programs.alacritty = import programs/alacritty config pkgs;
    programs.fish = import programs/fish;
    programs.kitty = import programs/kitty config pkgs;
    programs.noti.enable = true;
    programs.urxvt = import programs/urxvt config pkgs;
    programs.vscode = import programs/vscode config pkgs;

    xdg.configFile = import xdg/configFile config pkgs;
}
