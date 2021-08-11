i3status-exe: i3status-conf:

let color = border: background: text: {
        inherit border background text;
    };
in

[{
    statusCommand = "${i3status-exe} ${i3status-conf}";
    colors = {
        statusline = "#586e75";
        background = "#fdf6e3";
        focusedWorkspace  = color "#93a1a1" "#859900" "#fdf6e3";
        activeWorkspace   = color "#93a1a1" "#859900" "#002b36";
        inactiveWorkspace = color "#93a1a1" "#fdf6e3" "#002b36";
        urgentWorkspace   = color "#93a1a1" "#dc322f" "#fdf6e3";
        bindingMode       = color "#93a1a1" "#dc322f" "#fdf6e3";
    };
    workspaceNumbers = false;
}]
