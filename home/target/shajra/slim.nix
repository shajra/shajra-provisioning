{ lib, pkgs, ... }:

{
    imports = [ ./. ];
    home.extraPackages = lib.mkForce [];
}
