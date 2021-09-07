config: pkgs:

{
    enable = true;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.gnome3.adwaita-icon-theme;
    settings = {
        global = {
            browser = "${config.programs.firefox.package}/bin/firefox";
            corner_radius = 24;
            dmenu = "${config.programs.rofi.package}/bin/rofi -dmenu";
            ellipsize = "end";
            font = "Source Serif Pro";
            format = "<b>%s</b> %b";
            frame_width = 8;
            geometry = "1200x0-50+100";
            horizontal_padding = 24;
            markup = "full";
            min_icon_size = 64;
            max_icon_size = 64;
            padding = 12;
            progress_bar_frame_width = 2;
            progress_bar_height = 24;
            separator_height = 8;
            shrink = true;
            text_icon_padding = 8;
            transparency = 5;
        };
        urgency_low = {
            background  = "#fdf6e3";
            foreground  = "#657b83";
            frame_color = "#859900";
            highlight   = "#657b83";
        };
        urgency_normal  = {
            background  = "#fdf6e3";
            foreground  = "#859900";
            frame_color = "#859900";
            highlight   = "#859900";
        };
        urgency_critical = {
            background  = "#fdf6e3";
            foreground  = "#cb4b16";
            frame_color = "#859900";
            highlight   = "#cb4b16";
            timeout     = 0;
        };
        transient_history_ignore = {
            match_transient = true;
            history_ignore = true;
        };
        notify-time = {
            appname = "notify-time";
            format = "<big><b>%s</b></big>\\n<span foreground='#657b83'>%b</span>";
            timeout = 0;
        };
    };
}
