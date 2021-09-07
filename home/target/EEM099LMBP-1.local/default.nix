{ lib, ... }:

let
    build = import ../../.. {};
    shared = build.home.shared;
in

{
    imports = [ ../../modules/darwin ];

    home.file = import home/file shared;
    home.homeDirectory = shared."EEM099LMBP-1.local".homeDirectory;
    home.username = shared."EEM099LMBP-1.local".username;
    programs.alacritty.settings.font.size = lib.mkForce 18.0;
    programs.git = import programs/git lib;
    programs.kitty.extraConfig = lib.mkForce "font_size 18";
}
