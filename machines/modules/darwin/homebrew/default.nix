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
        "homebrew/cask-fonts"
        "homebrew/cask-versions"
        "homebrew/services"
        "koekeishiya/formulae"
        "felixkratz/formulae"
    ];

    brews = [
        # REVISIT: Darwin WM with Homebrew versus Nix
        #
        # Borders returns borders which were removed in MacOS 14.
        # Unfortunately, the latest MacOS SDK in Nixpkgs is still SDK 11.
        # Nixpkgs has the latest Yabai, but patches it slightly to keep
        # compilation with SDK 11. Both Sketchybar and Skhd are in Nixpkgs with
        # no patching.  Consequently, Nix-Darwin supports management of
        # Sketchybar, Skhd, and Yabai, but not Borders.
        #
        # Rather than split the installation of Darwin window management across
        # both Homebrew and Nixpkgs, it seems easier to leave all these four
        # packages with Homebrew.
        #
        # See https://github.com/NixOS/nixpkgs/issues/101229.
        #
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

        #     ❯ readlink -f "$(which sketchybar)"
        #     /opt/homebrew/Cellar/sketchybar/HEAD-e2009ab/bin/sketchybar
        #
        #     ❯ brew uninstall --force sketchybar
        #     ❯ brew install sketchybar
        #
        #     ❯ readlink -f "$(which sketchybar)"
        #     /opt/homebrew/Cellar/sketchybar/2.19.6/bin/sketchybar
        #
        { name =  "FelixKratz/formulae/borders";    args = ["HEAD"]; restart_service = "changed"; }
        { name =  "FelixKratz/formulae/sketchybar"; args = ["HEAD"]; restart_service = "changed"; }
        { name = "koekeishiya/formulae/skhd";       args = ["HEAD"]; }
        { name = "koekeishiya/formulae/yabai";      args = ["HEAD"]; }

        # DESIGN: not needed for now
        #"kubernetes-cli"
    ];

    casks = [
        "dash"
        "firefox"
        "google-chrome-beta"
        "microsoft-edge"
        "microsoft-edge-dev"

        # DESIGN: not needed for now
        #"docker"
        #"openvpn-connect"
        #"virtualbox"
        #"virtualbox-extension-pack"
    ];

    # DESIGN: homebrew.masApps seems a bit too hacky

    # REVISIT: installed manually; haven't figured out with either Homebrew or
    # Nix
    #
    # /Applications/'BluOS Controller.app'
    # /Applications/'Chrome Remote Desktop Host Uninstaller.app'
    # /Applications/DDPM
    # /Applications/Dell
    # /Applications/'HP Smart.app'
    # /Applications/'Jabra Direct.app'
    # /Applications/'Jabra Firmware Update.app'
    # /Applications/'Jump Desktop.app'
    # /Applications/'KensingtonWorks .app'
    # /Applications/'Okta Verify.app'
    # /Applications/'OpenVPN Connect.app'
    # /Applications/'TigerVNC Viewer 1.11.0.app'
    # /Applications/'Wacom Tablet.localized'
    # /Applications/WireGuard.app
}
