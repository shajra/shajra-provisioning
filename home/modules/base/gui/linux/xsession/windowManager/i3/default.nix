config: pkgs: lib:

let
    autorandr-exe = "${pkgs.autorandr}/bin/autorandr";
    i3msg-exe  = "${config.xsession.windowManager.i3.package}/bin/i3-msg";
    i3status-exe  = "${config.programs.i3status-rust.package}/bin/i3status-rs";
    kitty-exe = "${config.programs.kitty.package}/bin/kitty";
    hsetroot-exe  = "${pkgs.hsetroot}/bin/hsetroot";
    i3-conf = "${config.xdg.configHome}/i3status-rust/config-bottom.toml";
    i3status-conf = "${config.xdg.configHome}/i3status-rust/config-bottom.toml";
    runOnce = cmd: { command = cmd; always = false; notification = false; };
    modifierAlt = "Mod1";

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

    fontName = config.theme.fonts.proportional.name;
    modifier = "Mod4";

in

{
    enable = true;
    config = {
        inherit modifier;
        bars = import ./bars.nix i3status-exe i3status-conf colors foreground fontName;
        colors = import ./colors.nix colors;
        fonts = {
            names = ["${fontName}" ];
            size = 9.0;
        };
        gaps = import ./gaps.nix;
        floating.criteria = [ { class = "Pavucontrol"; } ];
        keybindings = import ./keybindings.nix
            config pkgs modifier modifierAlt kitty-exe;
        modes = import ./modes.nix lib modifier;
        startup = [
            (runOnce "${i3msg-exe} workspace 1")
            (runOnce "${autorandr-exe} --change --default home")
            (runOnce "${hsetroot-exe} -solid '${colors.semantic.background}'")
        ];
        terminal = kitty-exe;
        window = {
            border = 3;
            titlebar = false;
        };
    };
}
