{
    enable = true;
    onActivation.cleanup = "zap";
    global = {
        brewfile = true;
    };

    taps =  [
        "homebrew/bundle"
        "homebrew/cask"
        "homebrew/cask-drivers"
        "homebrew/cask-fonts"
        "homebrew/cask-versions"
        "homebrew/core"
        "homebrew/services"
        "koekeishiya/formulae"
    ];

    brews = [
        "skhd"
        "yabai"
        "svn"  # DESIGN: needed for Source Code Pro

        # DESIGN: not needed for now
        #"kubernetes-cli"
    ];

    casks = [
        "discord"
        "firefox"
        "font-awesome-terminal-fonts"
        "font-fontawesome"
        "font-source-code-pro"
        "google-chrome-beta"
        "iterm2"
        "karabiner-elements"
        "microsoft-edge"
        "slack"
        "wireshark"
        "zoom"

        # DESIGN: wanted, but broken for M1
        #"kensington-trackball-works"

        # DESIGN: not needed for now
        #"docker"
        #"openvpn-connect"
        #"virtualbox"
        #"virtualbox-extension-pack"
    ];

    # DESIGN: In case more cutting edge versions are needed
    #extraConfig = ''
    #    brew "koekeishiya/formulae/yabai", args: ["HEAD"], restart_service: :changed
    #    brew "koekeishiya/formulae/skhd", args: ["HEAD"], restart_service: :changed
    #'';
}
