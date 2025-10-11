{ config, pkgs, ... }:
let
  domain = import ./domain.nix;
in
{

  security.acme = {
    acceptTerms = true;
    defaults = {
      dnsProvider = "cloudflare";
      credentialFiles.CLOUDFLARE_DNS_API_TOKEN_FILE = "/run/secrets/acme/cloudflare/apiToken";
      group = config.services.nginx.group;
    };
    certs = pkgs.lib.mapAttrs' (_k: v: pkgs.lib.nameValuePair v { }) domain;
  };

  services = {
    immich.secretsFile = config.sops.templates."immich.env".path;
    vaultwarden.environmentFile = config.sops.templates."vaultwarden.env".path;
  };

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
          from = config.sops.placeholder."smtp/username";
          replyTo = config.sops.placeholder."smtp/username";
          transport = {
            host = "smtp.improvmx.com";
            username = config.sops.placeholder."smtp/username";
            password = config.sops.placeholder."smtp/password";
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
    "vaultwarden.env" = {
      restartUnits = [ config.systemd.services.vaultwarden.name ];
      content = ''
        SMTP_FROM=${config.sops.placeholder."smtp/username"}
        SMTP_USERNAME=${config.sops.placeholder."smtp/username"}
        SMTP_PASSWORD=${config.sops.placeholder."smtp/password"}
      '';
    };
  };
}
