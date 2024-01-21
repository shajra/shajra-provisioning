{ config, lib, pkgs, build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
    ];

    disabledModules = [ "targets/darwin/linkapps.nix" ];
    home.activation = import home/activation config lib pkgs;
    home.extraPackages = build.pkgs.lists.base.tui.darwin;

    # REVISIT: 23-12-21: pinned to stable because broot built was broken
    programs.broot.package = build.infra.np.nixpkgs.stable.broot;

    xdg.configFile = import xdg/configFile;
}
