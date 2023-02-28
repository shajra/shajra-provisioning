{ config, pkgs, build, ... }:

let

    infra = build.infra;
    hostname = "bagel";
    user = build.config.provision.user."${hostname}".username;

in {
    # DESIGN: for when the day Yabai builds in Nix again
    #environment.etc."sudoers.d/yabai".text = ''
    #    shajra ALL = (root) NOPASSWD: ${pkgs.yabai}/bin/yabai --load-sa
    #'';
    environment.etc."sudoers.d/yabai".text = ''
        shajra ALL = (root) NOPASSWD: /opt/homebrew/Cellar/yabai/*/bin/yabai --load-sa
    '';

    environment.systemPackages = [];

    homebrew = import ./homebrew;

    networking.hostName = hostname;

    nix.configureBuildUsers = true;
    nix.extraOptions = ''
        experimental-features = nix-command flakes
    '';
    nix.package = pkgs.nixFlakes;
    nix.settings.substituters = [
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
    nix.settings.trusted-users = [ "root" user ];

    # DESIGN: Don't think this is needed with flakes
    #nixpkgs.config = infra.np.config;
    #nixpkgs.overlays = infra.np.overlays;

    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableSSHSupport = true;
    programs.zsh.enable = true;
    programs.fish.enable = true;

    services.nix-daemon.enable = true;
    services.skhd.enable = false;  # DESIGN: broken for M1
    services.skhd.package = pkgs.skhd;
    services.yabai.enable = false;  # DESIGN: broken for M1
    services.yabai.package = pkgs.yabai;

    system.activationScripts.postUserActivation.text = ''
        # DESIGN: an example of useful arbitrary Mac cleanup
        #echo removing Chromium from quarantine...
        #xattr -dr com.apple.quarantine /Applications/Chromium.app
    '';

    system.checks.verifyNixPath = false;
    system.stateVersion = 4;

}
