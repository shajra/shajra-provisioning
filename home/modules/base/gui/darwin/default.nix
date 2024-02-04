{ lib, config, pkgs, build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
        ../../tui/darwin
    ];

    home.extraPackages = build.pkgs.lists.base.gui.darwin;

    launchd = import ./launchd config pkgs;

    programs.fish = import programs/fish pkgs;
    programs.kitty = import programs/kitty config;

    xdg.configFile = import xdg/configFile config pkgs;
}
