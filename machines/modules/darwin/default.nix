{ pkgs, ... }:

{
    # DESIGN: for when the day Yabai builds in Nix again
    #environment.etc."sudoers.d/yabai".text = ''
    #    shajra ALL = (root) NOPASSWD: ${pkgs.yabai}/bin/yabai --load-sa
    #'';
    environment.etc."sudoers.d/yabai".text = ''
        shajra ALL = (root) NOPASSWD: /opt/homebrew/Cellar/yabai/*/bin/yabai --load-sa
    '';

    environment.systemPackages = [];

    homebrew = import ./homebrew;

    nix.configureBuildUsers = true;

    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableSSHSupport = true;
    programs.zsh.enable = true;

    # DESIGN: so far, don't really need it
    services.karabiner-elements.enable = false;

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
    system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
    system.stateVersion = 4;

}
