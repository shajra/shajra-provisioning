{ config, pkgs, build, ... }:

let kitty = build.infra.np.nixpkgs.master.kitty;
in {
    imports = [
        ../../../ubiquity
        ../all
        ../../tui/darwin
    ];

    home.extraPackages = build.pkgs.lists.base.gui.darwin;

    programs.fish = import programs/fish pkgs;
    programs.kitty = import programs/kitty config kitty;

    xdg.configFile = import xdg/configFile config pkgs;
}
