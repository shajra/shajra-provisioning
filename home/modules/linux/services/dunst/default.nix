config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

in

{
    enable = true;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.gnome3.adwaita-icon-theme;
    settings = {
        global = {
            browser = "${config.programs.firefox.package}/bin/firefox";
            corner_radius = 24;
            dmenu = "${config.programs.rofi.package}/bin/rofi -dmenu -p action";
            ellipsize = "end";
            font = config.theme.fonts.proportional.name;
            format = "<b>%s</b> %b";
            frame_width = 8;
            geometry = "1200x0-50+125";
            horizontal_padding = 24;
            markup = "full";
            max_icon_size = 64;
            min_icon_size = 64;
            mouse_left_click = "close_current";
            mouse_middle_click = "close_all";
            mouse_right_click = "do_action, close_current";
            padding = 12;
            progress_bar_frame_width = 2;
            progress_bar_height = 24;
            separator_height = 8;
            shrink = true;
            text_icon_padding = 8;
            transparency = 5;
        };
        urgency_low = rec {
            background  = colors.semantic.background;
            foreground  = colors.semantic.foreground;
            frame_color = colors.semantic.unifying;
            highlight   = foreground;
        };
        urgency_normal  = {
            background  = colors.semantic.background;
            foreground  = colors.semantic.unifying;
            frame_color = colors.semantic.unifying;
            highlight   = colors.semantic.unifying;
        };
        urgency_critical = {
            background  = colors.semantic.background;
            foreground  = colors.semantic.urgent;
            frame_color = colors.semantic.unifying;
            highlight   = colors.semantic.urgent;
            timeout     = 0;
        };
        transient_history_ignore = {
            match_transient = true;
            history_ignore = true;
        };
        notify-time = {
            appname = "notify-time";
            format = "<big><b>%s</b></big>\\n<span foreground='${colors.semantic.foreground}'>%b</span>";
            timeout = 0;
        };
    };
}
