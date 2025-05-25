config: pkgs:

let
    makeLink = config.lib.file.mkOutOfStoreSymlink;
in

{
    ".XCompose".text = import xcompose/text.nix pkgs.sources;
    ".Xdefaults".source = makeLink "${config.home.homeDirectory}/.Xresources";

    # DESIGN: Ideally, Home Manager and Nixpkgs would make this easier
    # https://github.com/NixOS/nixpkgs/issues/47340
    ".mozilla/native-messaging-hosts/tridactyl.json".source =
        "${config.programs.firefox.finalPackage}/lib/mozilla/native-messaging-hosts/tridactyl.json";
}
