i3status-exe: i3status-conf: colors: _foreground: fontName:

[
  {
    fonts = {
      names = [
        "${fontName}"
        "Font Awesome 6 Free"
        "Symbols Nerd Font"
      ];
      size = 9.0;
    };
    statusCommand = "${i3status-exe} ${i3status-conf}";
    colors = {
      statusline = colors.semantic.foreground;
      inherit (colors.semantic) background;
      focusedWorkspace = {
        inherit (colors.window.selected.focused) background;
        inherit (colors.window.selected.focused) text;
        border = colors.window.selected.focused.border.workspace;
      };
      activeWorkspace = {
        inherit (colors.window.selected.unfocused) background;
        inherit (colors.window.selected.unfocused) text;
        border = colors.window.selected.unfocused.border.workspace;
      };
      inactiveWorkspace = {
        inherit (colors.window.unselected) background;
        inherit (colors.window.unselected) text;
        border = colors.window.unselected.border.workspace;
      };
      urgentWorkspace = {
        inherit (colors.window.urgent) background;
        inherit (colors.window.urgent) text;
        border = colors.window.urgent.border.workspace;
      };
      bindingMode = {
        inherit (colors.window.bindingMode) background;
        inherit (colors.window.bindingMode) text;
        inherit (colors.window.bindingMode) border;
      };
    };
    position = "top";
    workspaceNumbers = false;
  }
]
