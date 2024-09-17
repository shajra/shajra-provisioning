{
    enable = true;
    settings = {
        cmd_duration.show_notifications = false;
        status = {
            disabled = false;
            format = "[$symbol $status]($style)";
        };
        custom.jj_branch = {
            command = "jj branch list -T 'if(!remote, name ++ \" \")'";
            require_repo = true;
            when = "test -d .jj";
            description = "Show JJ branches";
            format = "on [$symbol$output]($style) ";
            style = "bold purple";
            symbol = " ";
        };
    };
}
