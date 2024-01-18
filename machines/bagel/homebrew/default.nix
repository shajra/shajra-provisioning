{
    enable = true;
    onActivation.cleanup = "zap";
    global = {
        brewfile = true;
    };

    taps =  [
        "homebrew/bundle"
        "homebrew/cask-drivers"
        "homebrew/cask-fonts"
        "homebrew/cask-versions"
        "homebrew/services"
        "koekeishiya/formulae"
        "FelixKratz/formulae"
    ];

    brews = [
        "borders"
        "sketchybar"
        "skhd"
        "yabai"

        # DESIGN: not needed for now
        #"kubernetes-cli"
    ];

    casks = [
        # DESIGN: browsers hard to support in Nixpkgs for Darwin
        "firefox"
        "google-chrome-beta"
        "microsoft-edge"
        "microsoft-edge-beta"

        # DESIGN: not needed for now
        #"docker"
        #"openvpn-connect"
        #"virtualbox"
        #"virtualbox-extension-pack"
    ];

    # DESIGN: installed manually; haven't figured out with either Brew or Nix
    # /Applications/'BluOS Controller.app'
    # /Applications/'Chrome Remote Desktop Host Uninstaller.app'
    # /Applications/DDPM
    # /Applications/Dell
    # /Applications/'HP Smart.app'
    # /Applications/'Jabra Direct.app'
    # /Applications/'Jabra Firmware Update.app'
    # /Applications/'Jump Desktop.app'
    # /Applications/Karabiner-Elements.app
    # /Applications/Karabiner-EventViewer.app
    # /Applications/'KensingtonWorks .app'
    # /Applications/'Okta Verify.app'
    # /Applications/'OpenVPN Connect.app'
    # /Applications/'TigerVNC Viewer 1.11.0.app'
    # /Applications/'Wacom Tablet.localized'
    # /Applications/WireGuard.app

    # DESIGN: In case more cutting edge versions are needed
    #extraConfig = ''
    #    brew "koekeishiya/formulae/yabai", args: ["HEAD"], restart_service: :changed
    #    brew "koekeishiya/formulae/skhd", args: ["HEAD"], restart_service: :changed
    #'';
}
