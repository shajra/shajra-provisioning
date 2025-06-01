{ config, pkgs, build, ... }:

let pkgs-unstable = build.infra.np.nixpkgs.unstable;
in {
    imports = [
        ../../../ubiquity
        ../all
        ../../tui/darwin
    ];

    programs.alacritty = import programs/alacritty;
    programs.fish = import programs/fish pkgs;
    programs.kitty = import programs/kitty config;

    xdg.configFile = import xdg/configFile pkgs;
}
