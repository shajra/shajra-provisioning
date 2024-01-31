{ config, pkgs, lib, build, ... }:

let
    userConfig = build.config.provision.user;
    hostname = "shajra";
in

{
    imports = [
        ../../modules/base/tui/linux
    ];

    home.file = import home/file config;
    home.homeDirectory = userConfig."${hostname}".homeDirectory;
    home.username = userConfig."${hostname}".username;

    nix.package = pkgs.nix;  # Only used for configuration generation
    nix.settings.extra-experimental-features = ["nix-command" "flakes"];
    nix.settings.extra-substituters = [
        "https://shajra.cachix.org"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://haskell-language-server.cachix.org"
        "https://nix-community.cachix.org"
    ];
    nix.settings.trusted-public-keys = [
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    programs.fish = import programs/fish;
    programs.git = import programs/git lib;

    targets.genericLinux.enable = true;

    xdg.configFile = import xdg/configFile config;
}
