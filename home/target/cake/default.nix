{
  config,
  pkgs,
  build,
  ...
}:

let
  userConfig = build.config.provision.user;
in

{
  imports = [
    ../../modules/base/gui/linux
    ../../modules/centralized

    ../../modules/audio/gui
    ../../modules/chat/gui
    ../../modules/documentation/linux
    ../../modules/finance

    ../../modules/programming/c
    ../../modules/programming/cloud
    ../../modules/programming/db
    ../../modules/programming/general
    ../../modules/programming/go
    ../../modules/programming/haskell
    ../../modules/programming/java
    ../../modules/programming/lua
    ../../modules/programming/python
    ../../modules/programming/racket
    # DESIGN: ruby's bin/bundle conflicts with gotools's
    #../../modules/programming/ruby
    ../../modules/programming/rust
    ../../modules/programming/scala
    ../../modules/programming/shell
    ../../modules/programming/web

    ../../modules/sync
    ../../modules/video/linux
  ];

  home.homeDirectory = userConfig.cake.homeDirectory;
  home.username = userConfig.cake.username;
  programs.alacritty.settings.font.size = 11.0;
  programs.fish = import programs/fish pkgs;
  programs.i3status-rust = import programs/i3status-rust config pkgs;
  programs.kitty.extraConfig = "font_size 11";
}
