{ lib, build, ... }:

let
    userConfig = build.config.provision.user;
    hostname = "lemon";
in

{
    imports = [
        ../../modules/base/gui/darwin

        ../../modules/audio/gui/all
        ../../modules/audio/tui/all

        ../../modules/chat/gui/all
        ../../modules/chat/tui/all

        ../../modules/documentation/all

        ../../modules/programming/c/all
        ../../modules/programming/containers
        ../../modules/programming/db
        ../../modules/programming/general
        #../../modules/programming/haskell  # long build, not cached
        ../../modules/programming/java
        ../../modules/programming/lua
        ../../modules/programming/python
        ../../modules/programming/scala
        ../../modules/programming/shell

        #../../modules/sync  # Ocaml not built/cached

        ../../modules/video/all
    ];

    home.file = import home/file userConfig hostname;
    home.homeDirectory = userConfig."${hostname}".homeDirectory;
    home.username = userConfig."${hostname}".username;
    programs.alacritty.settings.font.size = 18.0;
    programs.kitty.extraConfig = "font_size 18";
    xdg.configFile = import xdg/configFile lib;
}
