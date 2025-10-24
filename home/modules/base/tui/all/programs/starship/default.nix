{
  enable = true;
  settings = {
    cmd_duration.show_notifications = false;
    gcloud.format = "[$symbol$active]($style) ";
    status = {
      disabled = false;
      format = "[$symbol $status]($style)";
    };
    custom.jj_branch = {
      command = "jj bookmark list -r ::@ -T 'if(!remote, name ++ \" \")'";
      require_repo = true;
      when = "test -d .jj";
      description = "Show JJ branches";
      format = "on [$symbol$output]($style) ";
      style = "bold purple";
      symbol = " ";
    };
  };
}
