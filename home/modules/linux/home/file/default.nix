config: pkgs: sources:

let
    makeLink = link: pkgs.runCommand "makeLink" {} "ln -s ${link} $out";
in

{
    ".nix-channels".source = ./nix-channels;
    ".XCompose".text = import xcompose/text.nix sources;
    ".Xdefaults".source = makeLink "${config.home.homeDirectory}/.Xresources";

    # DESIGN: Ideally, Home Manager and Nixpkgs would make this easier
    # https://github.com/NixOS/nixpkgs/issues/47340
    ".mozilla/native-messaging-hosts/tridactyl.json".source =
        "${pkgs.firefox}/lib/mozilla/native-messaging-hosts/tridactyl.json";
}
