i3status-exe: i3status-conf: colors: foreground: fontName:

[{
    fonts = ["Material Icons 20" "${fontName} 9"];
    statusCommand = "${i3status-exe} ${i3status-conf}";
    colors = {
        statusline = colors.semantic.foreground;
        background = colors.semantic.background;
        focusedWorkspace  = {
            background = colors.window.selected.focused.background;
            text       = colors.window.selected.focused.text;
            border     = colors.window.selected.focused.border.workspace;
        };
        activeWorkspace   = {
            background = colors.window.selected.unfocused.background;
            text       = colors.window.selected.unfocused.text;
            border     = colors.window.selected.unfocused.border.workspace;
        };
        inactiveWorkspace = {
            background = colors.window.unselected.background;
            text       = colors.window.unselected.text;
            border     = colors.window.unselected.border.workspace;
        };
        urgentWorkspace   = {
            background = colors.window.urgent.background;
            text       = colors.window.urgent.text;
            border     = colors.window.urgent.border.workspace;
        };
        bindingMode       = {
            background = colors.window.bindingMode.background;
            text       = colors.window.bindingMode.text;
            border     = colors.window.bindingMode.border;
        };
    };
    position = "top";
    workspaceNumbers = false;
}]
