{ config, pkgs, build, ... }:

let
    userConfig = build.config.provision.user;
in

{
    imports = [
        ../../modules/base/gui/linux

        ../../modules/audio/gui
        ../../modules/centralized
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
    programs.password-store.enable = true;

    accounts.email.accounts.gmail = {
        flavor = "gmail.com";
        lieer.enable = true;
        lieer.settings.account = "me";
        lieer.settings.ignore_tags = [ "new" ];
        lieer.sync.enable = true;
        notmuch.enable = true;
        primary = true;
        realName = "Do Not Reply";
    };
    accounts.email.maildirBasePath = "${userConfig.cake.homeDirectory}/var/mail";

    programs.lieer.enable = true;
    programs.notmuch.enable = true;
    programs.notmuch.extraConfig.index."header.List" = "List-Id";
    programs.notmuch.extraConfig.index."header.SneakAddr" = "X-Sneakemail-Address";
    programs.notmuch.extraConfig.index."header.SneakFrom" = "X-Sneakemail-From";
    programs.notmuch.new.tags = [ "new" ];

    #services.lieer.enable = true;
}
