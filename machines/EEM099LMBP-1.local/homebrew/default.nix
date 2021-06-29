{
    enable = true;
    cleanup = "zap";
    global = {
        brewfile = true;
        noLock = true;
    };

    taps =  [
        "homebrew/bundle"
        "homebrew/cask"
        "homebrew/cask-drivers"
        "homebrew/cask-fonts"
        "homebrew/core"
        "homebrew/services"
    ];

    brews = [
        "kubernetes-cli"
    ];

    casks = [
        "chromium"
        "docker"
        "firefox"
        "flash-npapi"
        "font-awesome-terminal-fonts"
        "font-fontawesome"
        "font-source-code-pro"
        "iterm2"
        "karabiner-elements"
        "kensington-trackball-works"
        "openvpn-connect"
        "slack"
        "virtualbox"
        "virtualbox-extension-pack"
        "wireshark"
        "zoom"
    ];

}
