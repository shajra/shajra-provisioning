config: kitty:

{
    # DESIGN: waiting on PR #166661 to hit nixpkgs-unstable
    package = kitty;

    darwinLaunchOptions = [
        "--single-instance"
        "--wait-for-single-instance-window-close"
        "--directory=${config.home.homeDirectory}"
    ];
}
