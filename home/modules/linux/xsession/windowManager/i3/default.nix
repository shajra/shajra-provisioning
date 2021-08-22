config: pkgs:

let
    kitty-exe = "${pkgs.kitty}/bin/kitty";
    autorandr-exe = "${pkgs.autorandr}/bin/autorandr";
    i3msg-exe  = "${config.xsession.windowManager.i3.package}/bin/i3-msg";
    i3status-exe  = "${config.programs.i3status-rust.package}/bin/i3status-rs";
    i3-conf = "${config.xdg.configHome}/i3status-rust/config-bottom.toml";
    i3status-conf = "${config.xdg.configHome}/i3status-rust/config-bottom.toml";
    runOnce = cmd: { command = cmd; always = false; notification = false; };
in

{
    enable = true;
    config = rec {
        bars = import ./bars.nix i3status-exe i3status-conf;
        colors = import ./colors.nix;
        fonts = ["Source Serif Pro 10"];
        floating.criteria = [ { class = "Pavucontrol"; } ];
        keybindings = import ./keybindings.nix modifier config pkgs kitty-exe;
        modes = import ./modes.nix modifier;
        modifier = "Mod4";
        startup = [
            (runOnce "${i3msg-exe} workspace 1")
            (runOnce "${autorandr-exe} --change --default home")
        ];
        terminal = kitty-exe;
        window = {
            border = 3;
        };
    };
}
