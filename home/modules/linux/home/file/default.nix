sources:

{
    ".nix-channels".source = ./nix-channels;
    ".XCompose".text = import xcompose/text.nix sources;
    ".xprofile".source = xprofile/xprofile.libinput;
}
