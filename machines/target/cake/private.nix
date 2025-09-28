{ config, ... }:
let
  domain = import ./domain.nix;
in
{
  services.immich.secretsFile = config.sops.templates."immich.env".path;
  sops.templates = {
    "immich.env" = {
      restartUnits = [ config.systemd.services.immich-server.name ];
      content = ''
        DB_PASSWORD=${config.sops.placeholder."immich/db/password"}
        IMMICH_CONFIG_FILE=${config.sops.templates."immich-config.json".path}
      '';
    };
    "immich-config.json" = {
      owner = config.services.immich.user;
      restartUnits = [ config.systemd.services.immich-server.name ];
      content = builtins.toJSON {
        notifications.smtp = {
          enabled = true;
          from = config.sops.placeholder."immich/smtp/username";
          replyTo = config.sops.placeholder."immich/smtp/replyTo";
          transport = {
            host = "smtp.improvmx.com";
            username = config.sops.placeholder."immich/smtp/username";
            password = config.sops.placeholder."immich/smtp/password";
          };
        };
        server.externalDomain = "https://${domain.immich}";
        storageTemplate = {
          enabled = true;
          hashVerificationEnabled = true;
          template = "{{y}}/{{#if album}}{{album}}{{else}}Other{{/if}}/{{MM}}/{{filename}}";
        };
      };
    };
  };
}
