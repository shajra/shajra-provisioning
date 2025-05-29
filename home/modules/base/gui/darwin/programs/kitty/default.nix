config:

{
    darwinLaunchOptions = [
        "--single-instance"
        "--wait-for-single-instance-window-close"
        "--directory=${config.home.homeDirectory}"
    ];
    settings = {
        macos_option_as_alt = "left";
        exe_search_path = "${config.home.homeDirectory}/.nix-profile/bin";
    };
}
