config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    inherit (config.theme.colors.nominal) foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;
    browser = "${config.programs.firefox.finalPackage}/bin/firefox-beta";

in

{
    enable = true;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.adwaita-icon-theme;
    settings = {
        global = {
            inherit browser;
            corner_radius = 12;
            dmenu = "${config.programs.rofi.package}/bin/rofi -dmenu -p action";
            ellipsize = "end";
            font = config.theme.fonts.proportional.name;
            format = "<b>%s</b> %b";
            frame_width = 3;
            origin = "top-right";
            offset = "24x78";
            horizontal_padding = 12;
            markup = "full";
            max_icon_size = 48;
            min_icon_size = 24;
            mouse_left_click = "close_current";
            mouse_middle_click = "close_all";
            mouse_right_click = "do_action, close_current";
            padding = 12;
            progress_bar_frame_width = 1;
            progress_bar_height = 9;
            separator_height = 3;
            shrink = true;
            text_icon_padding = 12;
            transparency = 5;
            vertical_alignment = "top";
        };
        urgency_low = rec {
            inherit (colors.semantic) background;
            inherit (colors.semantic) foreground;
            frame_color = colors.semantic.unifying;
            highlight   = foreground;
        };
        urgency_normal  = {
            inherit (colors.semantic) background;
            foreground  = colors.semantic.unifying;
            frame_color = colors.semantic.unifying;
            highlight   = colors.semantic.unifying;
        };
        urgency_critical = {
            inherit (colors.semantic) background;
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
