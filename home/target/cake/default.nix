{ config, pkgs, build, ... }:

let
    userConfig = build.config.provision.user;
in

{
    imports = [
        ../../modules/base/gui/linux

        ../../modules/audio/gui/all
        ../../modules/audio/tui/linux

        ../../modules/centralized

        ../../modules/chat/gui/linux

        ../../modules/documentation/linux

        ../../modules/finance

        ../../modules/os/nixos

        ../../modules/programming/c/linux
        ../../modules/programming/db
        ../../modules/programming/general
        ../../modules/programming/haskell
        ../../modules/programming/java
        ../../modules/programming/python
        ../../modules/programming/scala
        ../../modules/programming/shell

        ../../modules/sync

        ../../modules/video/linux
    ];

    home.file = import home/file config pkgs userConfig;
    home.homeDirectory = userConfig.cake.homeDirectory;
    home.username = userConfig.cake.username;
    programs.alacritty.settings.font.size = 11.0;
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
