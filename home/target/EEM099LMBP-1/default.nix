{ lib, ... }:

{
    imports = [ ../../modules/darwin ];

    home.file = import home/file;
    home.homeDirectory = "/Users/sukant";
    home.username = "sukant";
    programs.alacritty.settings.font.size = lib.mkForce 18.0;
    programs.git = import programs/git lib;
    programs.kitty.extraConfig = lib.mkForce "font_size 18";
}
