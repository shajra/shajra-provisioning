{ config, pkgs, lib, build, ... }:

let
    userConfig = build.config.provision.user;
    hostname = "shajra";
in

{
    imports = [
        ../../modules/base/tui/linux
        ../../modules/work
    ];

    home.file = import home/file config;
    home.homeDirectory = userConfig."${hostname}".homeDirectory;
    home.username = userConfig."${hostname}".username;

    nix.settings.extra-experimental-features = ["nix-command" "flakes"];
    nix.settings.extra-substituters = [
        "https://cache.garnix.io"
        "https://shajra.cachix.org"
        "https://nix-community.cachix.org"
        "https://haskell-language-server.cachix.org"
        "https://cache.iog.io"
    ];
    nix.settings.trusted-public-keys = [
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    programs.fish = import programs/fish config;

    targets.genericLinux.enable = true;

    xdg.configFile = import xdg/configFile config;
}
