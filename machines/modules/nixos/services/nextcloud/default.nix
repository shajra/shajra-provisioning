{ lib, ... }:

let

  certType =
    descExtra: default:
    lib.mkOption {
      type = lib.types.path;
      description = "Path to server SSL certificate${descExtra}.";
      inherit default;
    };

in
{

  options = {
    services.nextcloud.sslCertificate = certType "" ./dummy.crt;
    services.nextcloud.sslCertificateKey = certType " key" ./dummy.key;
  };
}
