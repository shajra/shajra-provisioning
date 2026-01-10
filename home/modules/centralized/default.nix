{ build, config, ... }:

let
  userConfig = build.config.provision.user;
in

{
  imports = [ ../ubiquity ];

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

  home.extraPackages = build.pkgs.lists.centralized;

  programs.newsboat.enable = true;
  programs.password-store.enable = true;

  programs.rclone.enable = true;
  programs.rclone.remotes = {
    google-tnks-public-drive = {
      config = {
        type = "drive";
        scope = "drive";
      };
      secrets = {
        client_id = "/run/secrets/rclone/google-tnks-public-drive/client_id";
        client_secret = "/run/secrets/rclone/google-tnks-public-drive/client_secret";
        token = "/run/secrets/rclone/google-tnks-public-drive/token";
      };
    };
  };

  programs.rofi.pass.enable = true;
  programs.rofi.pass.stores = [ "${config.home.homeDirectory}/src/live/password-store" ];

  programs.lieer.enable = true;

  programs.notmuch.enable = true;
  programs.notmuch.extraConfig.index."header.List" = "List-Id";
  programs.notmuch.extraConfig.index."header.SneakAddr" = "X-Sneakemail-Address";
  programs.notmuch.extraConfig.index."header.SneakFrom" = "X-Sneakemail-From";
  programs.notmuch.new.tags = [ "new" ];

  #services.lieer.enable = true;
}
