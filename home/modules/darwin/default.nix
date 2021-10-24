{ config, lib, pkgs, ... }:

{
    imports = [ ../base ];

    home.activation = import home/activation config lib pkgs;
    home.file = import home/file config pkgs;

    programs.emacs.package = lib.mkForce pkgs.emacsMacport;
}
