{ lib, ... }:

let

    certType = descExtra: default: lib.mkOption {
        type = lib.types.path;
        description = "Path to server SSL certificate${descExtra}.";
        inherit default;
    };

in {

    options = {
        services.mealie.sslCertificate = certType "" ./dummy.crt;
        services.mealie.sslCertificateKey = certType " key" ./dummy.key;
    };
}
