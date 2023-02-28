{ config, pkgs, build, ... }:

let kitty = build.infra.np.nixpkgs.master.kitty;
in {
    imports = [
        ../../../ubiquity
        ../all
        ../../tui/darwin
    ];

    home.file = import home/file config pkgs;
    home.extraPackages = build.pkgs.lists.base.tui.darwin;

    programs.fish = import programs/fish pkgs;
    programs.kitty = import programs/kitty config kitty;
}
