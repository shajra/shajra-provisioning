config: pkgs:

let
    alacritty-exe = "${config.programs.alacritty.package}/bin/alacritty";
    i3status-exe  = "${config.programs.i3status-rust.package}/bin/i3status-rs";
    i3status-conf = "${config.xdg.configHome}/i3status-rust/config-bottom.toml";
    runCmd = cmd: { command = cmd; always = true; };
in

{
    enable = true;
    config = rec {
        bars = import ./bars.nix i3status-exe i3status-conf;
        colors = import ./colors.nix;
        fonts = ["NotoSans Nerd Font Condensed Medium 10"];
        floating.criteria = [ { class = "Pavucontrol"; } ];
        keybindings = import ./keybindings.nix modifier config pkgs alacritty-exe;
        modes = import ./modes.nix modifier;
        modifier = "Mod4";
        startup = [(runCmd ". ~/.xprofile; systemctl --user start graphical-session.target")];
        terminal = alacritty-exe;
    };
}
