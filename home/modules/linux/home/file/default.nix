pkgs: sources:

{
    ".nix-channels".source = ./nix-channels;
    ".XCompose".text = import xcompose/text.nix sources;
    ".xprofile".source = xprofile/xprofile.libinput;

    # DESIGN: Ideally, Home Manager and Nixpkgs would make this easier
    # https://github.com/NixOS/nixpkgs/issues/47340
    ".mozilla/native-messaging-hosts/tridactyl.json".source =
        "${pkgs.firefox}/lib/mozilla/native-messaging-hosts/tridactyl.json";
}
