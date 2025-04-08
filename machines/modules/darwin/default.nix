{ build, config, pkgs, ... }:

let

    hostname = config.networking.hostName;
    superUser = build.config.provision.user."${hostname}".username;

    pkgs-unstable = build.infra.np.nixpkgs.unstable;

    format = pkgs.lib.colors.format "0xff%R%G%B";
    colors = pkgs.lib.colors.transformColors format config.theme.colors;

in {

    # REVISIT: for when the day Yabai builds in Nix again
    # shajra ALL = (root) NOPASSWD: ${pkgs.yabai}/bin/yabai --load-sa
    environment.etc."sudoers.d/yabai".text = ''
        ${superUser} ALL = (root) NOPASSWD: /opt/homebrew/Cellar/yabai/*/bin/yabai --load-sa
    '';

    environment.systemPackages = [];
    environment.systemPath = [ "/opt/homebrew/bin" ];

    fonts.packages = build.pkgs.lists.base.gui.all;

    homebrew = import ./homebrew;

    nix.configureBuildUsers = true;

    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableSSHSupport = true;
    programs.zsh.enable = true;

    services.jankyborders = import services/jankyborders pkgs-unstable colors;
    services.karabiner-elements.enable = false; # DESIGN: so far, don't really need it
    services.nix-daemon.enable = true;
    services.sketchybar = import services/sketchybar config pkgs pkgs-unstable colors;
    services.skhd = import services/skhd pkgs build colors;
    services.tailscale.enable = true;

    # REVISIT: Nixpkgs isn't building Yabai from source yet, and I want the
    # latest with patches
    #services.yabai = import services/yabai pkgs colors;

    system.activationScripts.postUserActivation.text = ''
        # DESIGN: an example of useful arbitrary Mac cleanup
        #echo removing Chromium from quarantine...
        #xattr -dr com.apple.quarantine /Applications/Chromium.app
    '';

    system.checks.verifyNixPath = false;

    system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
    system.defaults.NSGlobalDomain._HIHideMenuBar = true;
    system.defaults.dock.autohide = true;
    system.defaults.spaces.spans-displays = true;

    system.stateVersion = 5;
}
