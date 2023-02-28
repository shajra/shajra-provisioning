{ config, lib, pkgs, build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
    ];

    disabledModules = [ "targets/darwin/linkapps.nix" ];
    home.activation = import home/activation config lib pkgs;
    home.extraPackages = build.pkgs.lists.base.tui.darwin;

    # DESIGN: don't think this is needed any more
    #programs.macchina.package = build.pkgSets.base.tui.darwin.macchina;
}
