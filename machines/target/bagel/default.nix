{ config, pkgs, build, ... }:

let hostname = "bagel";
in {
    imports = [
        ../../modules/ubiquity
        ../../modules/darwin
    ];
    networking.hostName = hostname;
}
