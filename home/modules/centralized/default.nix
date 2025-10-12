{ build, config, ... }:

{
  imports = [ ../ubiquity ];
  home.extraPackages = build.pkgs.lists.centralized;
  programs.newsboat.enable = true;
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
}
