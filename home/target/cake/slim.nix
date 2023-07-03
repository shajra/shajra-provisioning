{ lib, pkgs, ... }:

{
    imports = [ ./. ];
    home.extraPackages = lib.mkForce [];
    accounts.email.accounts.gmail.address = "secret@secret.secret";
}
