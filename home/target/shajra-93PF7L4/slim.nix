{ lib, ... }:

{
  imports = [ ./. ];
  home.extraPackages = lib.mkForce [ ];
}
