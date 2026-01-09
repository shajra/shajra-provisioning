{ lib, build, ... }:

let
  userConfig = build.config.provision.user;
  hostname = "bagel";
in

{
  imports = [
    ../../modules/base/gui/darwin

    ../../modules/audio/gui
    ../../modules/chat/gui
    ../../modules/documentation/all
    ../../modules/laptop/darwin

    ../../modules/programming/c
    ../../modules/programming/db
    ../../modules/programming/general
    ../../modules/programming/lua
    ../../modules/programming/python
    ../../modules/programming/shell

    ../../modules/sync
    ../../modules/video/all
    ../../modules/work
  ];

  home.file = import home/file userConfig hostname;
  home.homeDirectory = userConfig."${hostname}".homeDirectory;
  home.username = userConfig."${hostname}".username;
  programs.alacritty.settings.font.size = 18.0;
  programs.kitty.extraConfig = "font_size 18";
  xdg.configFile = import xdg/configFile lib;
}
