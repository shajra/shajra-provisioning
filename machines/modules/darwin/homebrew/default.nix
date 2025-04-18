{
    enable = true;
    onActivation.cleanup = "zap";
    onActivation.autoUpdate = true;
    global = {
        autoUpdate = true;
        brewfile = true;
    };

    taps =  [
        "homebrew/bundle"
        "homebrew/services"
        "nikitabobko/tap"
    ];

    # NOTE: Remember that when using HEAD versions (sometimes needed for hot
    # fixes), Home Manager won't force Homebrew to recompile a package if
    # it's already installed.  To switch to/from HEAD, you'll have to
    # manually remove the package before calling darwin-rebuild.  Home
    # Manager is okay for initialization with Homebrew, but has notable
    # limitations when updating.
    #
    # With Homebrew, you can see the version installed by looking at the
    # filepath of the installation:
    #
    #     ❯ brew uninstall --force sketchybar
    #     ❯ brew install sketchybar --HEAD
    #
    #     ❯ readlink -f "$(which sketchybar)"
    #     /opt/homebrew/Cellar/sketchybar/HEAD-e2009ab/bin/sketchybar
    #
    #     ❯ brew uninstall --force sketchybar
    #     ❯ brew install sketchybar
    #
    #     ❯ readlink -f "$(which sketchybar)"
    #     /opt/homebrew/Cellar/sketchybar/2.19.6/bin/sketchybar

    brews = [
        # DESIGN: not needed for now
        #"kubernetes-cli"
    ];

    casks = [
        "dash"
        "firefox"
        "google-chrome@beta"
        "microsoft-edge@beta"

        # REVISIT: Aerospace doesn't build from source on Nix just yet.
        # What Nixpkgs does is just repackage a published binary release.
        # See https://github.com/NixOS/nixpkgs/issues/101229.
        "nikitabobko/tap/aerospace"
        "notion"
        "notion-enhanced"
        "slack"
        "tidal"
        "tor-browser"

        # DESIGN: using, but maybe not for long
        "docker"

        # DESIGN: not needed for now
        #"openvpn-connect"
        #"virtualbox"
        #"virtualbox-extension-pack"
    ];

    # DESIGN: homebrew.masApps seems a bit too hacky

    # REVISIT: installed manually; haven't figured out with either Homebrew or
    # Nix
    #
    # /Applications/'Chrome Remote Desktop Host Uninstaller.app'
    # /Applications/DDPM
    # /Applications/Dell
    # /Applications/'HP Smart.app'
    # /Applications/'Jabra Direct.app'
    # /Applications/'Jabra Firmware Update.app'
    # /Applications/'Jump Desktop.app'
    # /Applications/'KensingtonWorks .app'
    # /Applications/'Wacom Tablet.localized'
    # /Applications/WireGuard.app
}
