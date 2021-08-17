config: pkgs:

{
    enable = true;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.gnome3.adwaita-icon-theme;
    settings = {
        global = {
            corner_radius = 24;
            dmenu = "${config.programs.rofi.package}/bin/rofi";
            font = "SourceSerifPro: Bold";
            frame_width = 8;
            geometry = "0x0-50+100";
            horizontal_padding = 12;
            markup = "full";
            padding = 12;
            progress_bar_frame_width = 2;
            progress_bar_height = 24;
            separator_height = 8;
            text_icon_padding = 8;
            transparency = 10;
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
            timeout     = "0";
        };
    };
}
