pkgs-unstable: colors:

{
    enable = true;
    package = pkgs-unstable.jankyborders;
    style = "round";
    width = 8.0;
    hidpi = true;
    active_color = colors.window.selected.focused.border.window;
    inactive_color = colors.window.unselected.border.window;

}
