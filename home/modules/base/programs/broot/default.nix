config: pkgs:

let

    format = pkgs.lib.colors.format "rgb(%r, %g, %b)";
    colors = pkgs.lib.colors.transformColors format config.theme.colors;

in

{
    enable = true;
    skin = {

        # DESIGN: This is taken from broot source code (src/skin/style_map.rs)

        default = "${colors.semantic.foreground}"
            + " ${   colors.semantic.background}"
            + " / ${ colors.semantic.foreground_shadowed}"
            + " ${   colors.semantic.background_highlighted}";

        tree = "${colors.semantic.foreground_shadowed} none";

        parent = "${colors.semantic.foreground_shadowed} none";

        file      = "none none";
        directory = "${colors.nominal.blue} none bold";
        exe       = "${colors.nominal.red} none";
        link      = "${colors.nominal.magenta} none";
        pruning   = "${colors.semantic.foreground_shadowed} none italic";

        perm__    = "${colors.semantic.foreground_shadowed} none";
        perm_r    = "none none";
        perm_w    = "none none";
        perm_x    = "none none";

        owner     = "${colors.semantic.foreground_shadowed} none";
        group     = "${colors.semantic.foreground_shadowed} none";

        count = "${colors.nominal.yellow} none";  # RosyBrown      -> yellow
        dates = "${colors.nominal.cyan} none";    # PaleTurquoise4 -> cyan

        sparse = "none none";

        content_extract = "${colors.nominal.cyan} none";   # SpringGreen4 -> cyan
        content_match   = "${colors.nominal.green} none";  # Green3       -> green

        device_id_major = "${colors.nominal.yellow} none";  # RosyBrown -> yellow
        device_id_sep   = "${colors.semantic.foreground_shadowed} none"; # Grey53 -> base1
        device_id_minor = "${colors.nominal.yellow} none";  # RosyBrown -> yellow

        git_branch            = "${colors.semantic.foreground_emphasized} none";
        git_insertions        = "${colors.nominal.green} none";
        git_deletions         = "${colors.nominal.red} none";
        git_status_current    = "none none";
        git_status_modified   = "${colors.nominal.yellow} none";
        git_status_new        = "${colors.nominal.green} none";
        git_status_ignored    = "${colors.semantic.foreground_shadowed} none";
        git_status_conflicted = "${colors.nominal.red} none";
        git_status_other      = "${colors.nominal.red} none";

        selected_line = "none ${colors.semantic.background_highlighted}";
        char_match    = "${colors.nominal.green} none underlined";

        file_error = "${colors.nominal.orange} none italic";
        flag_label = "none none";
        flag_value = "${colors.nominal.yellow} none bold";
        input      = "none none";

        status_error    = "${colors.nominal.orange} ${colors.semantic.background_highlighted}";
        status_job      = "${colors.nominal.violet} ${colors.semantic.background_highlighted} bold";
        status_normal   = "none ${colors.semantic.background_highlighted}";
        status_italic   = "${colors.nominal.yellow} ${colors.semantic.background_highlighted} italic";
        status_bold     = "${colors.semantic.foreground_emphasized} ${colors.semantic.background_highlighted} bold";
        status_code     = "${colors.nominal.violet} ${colors.semantic.background_highlighted}";
        status_ellipsis = "none ${colors.semantic.background_highlighted}";

        purpose_normal   = "none ${colors.semantic.background_highlighted}";
        purpose_italic   = "${colors.nominal.yellow} ${colors.semantic.background_highlighted} italic";
        purpose_bold     = "${colors.semantic.foreground_emphasized} ${colors.semantic.background_highlighted} bold";
        purpose_ellipsis = "none ${colors.semantic.background_highlighted}";

        scrollbar_track = "${colors.semantic.background_highlighted} none";
        scrollbar_thumb = "none none";

        help_paragraph = "none none";
        help_bold    = "${colors.semantic.foreground_emphasized} none bold";
        help_italic  = "${colors.semantic.foreground_emphasized} none italic";
        help_code    = "${colors.semantic.foreground_emphasized} ${colors.semantic.background_highlighted}";
        help_headers = "${colors.nominal.yellow} none";
        help_table_border = "none none";

        preview = "${colors.semantic.foreground}"
            + " ${   colors.semantic.background}"
            + " / ${ colors.semantic.foreground_shadowed}"
            + " ${   colors.semantic.background_highlighted}";
        preview_title       = "${colors.semantic.foreground_shadowed} ${colors.semantic.background_highlighted}";
        preview_line_number = "${colors.semantic.foreground_shadowed} ${colors.semantic.background_highlighted}";
        preview_match = "None ${colors.nominal.green}";  # none SpringGreen4 -> green

        hex_null             = "${colors.semantic.foreground_emphasized} none";
        hex_ascii_graphic    = "${colors.nominal.cyan} none";
        hex_ascii_whitespace = "${colors.semantic.foreground_shadowed} none";
        hex_ascii_other      = "${colors.nominal.orange} none";
        hex_non_ascii        = "${colors.semantic.foreground_emphasized} none";

        staging_area_title = "none ${colors.semantic.background_highlighted}";

        mode_command_mark = "${colors.terminal.bright.white} ${colors.nominal.orange} bold";

    };
}
