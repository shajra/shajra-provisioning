i3status-exe: i3status-conf:

let color = border: background: text: {
        inherit border background text;
    };

    base03     = "#002b36";
    base02     = "#073642";
    base01     = "#586e75";  # emphasized text
    base00     = "#657b83";  # standard text
    base0      = "#839496";
    base1      = "#93a1a1";  # shadowed comments
    base2      = "#eee8d5";  # background highlights
    base3      = "#fdf6e3";  # background
    yellow     = "#b58900";
    orange     = "#cb4b16";
    red        = "#dc322f";
    magenta    = "#d33682";
    violet     = "#6c71c4";
    blue       = "#268bd2";
    cyan       = "#2aa198";
    green      = "#859900";

in

[{
    statusCommand = "${i3status-exe} ${i3status-conf}";
    colors = {
        statusline = base01;
        background = base3;
        focusedWorkspace  = color base1 green base3;
        activeWorkspace   = color base1 green base03;
        inactiveWorkspace = color base1 base3 base03;
        urgentWorkspace   = color base1 red   base3;
        bindingMode       = color base1 red   base3;
    };
    position = "top";
    workspaceNumbers = false;
}]
