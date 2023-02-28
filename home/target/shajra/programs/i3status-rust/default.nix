config: pkgs:

let

    daemon = "${pkgs.daemon}/bin/daemon";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    i3-dunst = "${pkgs.i3status-rust-dunst}/bin/i3status-rust-dunst";
    pkill = "${pkgs.procps}/bin/pkill";
    user = config.home.username;
    wpa_gui = "${pkgs.wpa_supplicant_gui}/bin/wpa_gui";

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

in

{
    bars = {
        bottom = {
            blocks = [
                {
                block = "focused_window";
                max_width = 70;
                show_marks = "visible";
                }
                {
                block = "disk_space";
                format = "{icon} {available} {percentage}";
                }
                {
                block = "memory";
                display_type = "memory";
                format_mem = "{mem_free_percents} {mem_avail_percents}";
                format_swap = "{swap_free_percents}";
                }
                {
                block = "cpu";
                interval = 3;
                }
                {
                block = "load";
                format = "{1m}";
                interval = 3;
                }
                {
                block = "net";
                device = "ens4";
                format = "{speed_up}  {speed_down}  {ip}";
                interval = 3;
                }
                {
                block = "notify";
                }
                {
                block = "custom";
                command  = "${i3-dunst} status";
                on_click = "${dunstctl} set-paused toggle; ${pkill} -u ${user} -SIGRTMIN+0 i3status-rs";
                signal = 0;
                hide_when_empty = true;
                interval = 3;
                json = true;
                }
                {
                block = "time";
                interval = 60;
                format = "%a %Y-%m-%d %l:%M %p";
                }
            ];
            icons = "awesome6";
            settings.theme = {
                overrides = {
                    idle_bg             = colors.semantic.unifying;
                    idle_fg             = foreground.semantic.unifying;
                    good_bg             = colors.semantic.good;
                    good_fg             = foreground.semantic.good;
                    info_bg             = colors.semantic.info;
                    info_fg             = foreground.semantic.info;
                    warning_bg          = colors.semantic.warning;
                    warning_fg          = foreground.semantic.warning;
                    critical_bg         = colors.semantic.urgent;
                    critical_fg         = foreground.semantic.urgent;

                    alternating_tint_bg = "#202020";  # lighten
                    alternating_tint_fg = "#202020";  # lighten

                    separator_bg        = "auto";
                    separator_fg        = "auto";
                    separator           = "";
                    #separator           = "";
                };
            };
        };
    };
}
