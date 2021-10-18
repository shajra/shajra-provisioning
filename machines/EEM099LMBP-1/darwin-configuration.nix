{ config, pkgs, ... }:

let

    infra = (import ../.. {}).infra;

in

{
    environment.etc."sudoers.d/yabai".text = ''
        sukant ALL = (root) NOPASSWD: ${pkgs.yabai}/bin/yabai --load-sa
    '';

    environment.systemPackages = [];

    homebrew = import ./homebrew;

    nix.binaryCaches = [
        "https://haskell-language-server.cachix.org"
        "https://hydra.iohk.io"
        "https://niv.cachix.org"
        "https://nix-community.cachix.org"
        "https://shajra.cachix.org"
    ];
    nix.binaryCachePublicKeys = [
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
    ];
    nix.trustedUsers = [ "root" "sukant" ];

    nixpkgs.config = infra.np.config;
    nixpkgs.overlays = infra.np.overlays;

    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableSSHSupport = true;
    programs.zsh.enable = true;
    programs.fish.enable = true;

    services.nix-daemon.enable = true;
    services.skhd.enable = true;
    services.skhd.package = pkgs.skhd;
    services.yabai.enable = true;
    services.yabai.package = pkgs.yabai;

    system.activationScripts.postUserActivation.text = ''
        echo removing Chromium from quarantine...
        xattr -dr com.apple.quarantine /Applications/Chromium.app
    '';

    system.checks.verifyNixPath = false;
    system.stateVersion = 4;

    users.nix.configureBuildUsers = true;
}
