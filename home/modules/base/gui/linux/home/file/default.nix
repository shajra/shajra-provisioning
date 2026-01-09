config: pkgs:

let
  makeLink = config.lib.file.mkOutOfStoreSymlink;
in

{
  ".XCompose".text = import xcompose/text.nix pkgs.shajra-sources;
  ".Xdefaults".source = makeLink "${config.home.homeDirectory}/.Xresources";

  # REVISIT: 2026-01-09: With the following ticket closed, maybe Home Manager
  # and Nixpkgs would make this easier:
  # https://github.com/NixOS/nixpkgs/issues/47340
  ".mozilla/native-messaging-hosts/tridactyl.json".source =
    "${config.programs.firefox.finalPackage}/lib/mozilla/native-messaging-hosts/tridactyl.json";
}
