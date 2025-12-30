colors:

{
  focused = {
    border = colors.window.border.tabs;
    inherit (colors.window.selected.focused) background;
    inherit (colors.window.selected.focused) text;
    inherit (colors.window.selected.focused) indicator;
    childBorder = colors.window.selected.focused.border.window;
  };
  focusedInactive = {
    border = colors.window.border.tabs;
    inherit (colors.window.selected.unfocused) background;
    inherit (colors.window.selected.unfocused) text;
    inherit (colors.window.selected.unfocused) indicator;
    childBorder = colors.window.selected.unfocused.border.window;
  };
  unfocused = {
    border = colors.window.border.tabs;
    inherit (colors.window.unselected) background;
    inherit (colors.window.unselected) text;
    inherit (colors.window.unselected) indicator;
    childBorder = colors.window.unselected.border.window;
  };
  urgent = {
    border = colors.window.border.tabs;
    inherit (colors.window.urgent) background;
    inherit (colors.window.urgent) text;
    inherit (colors.window.urgent) indicator;
    childBorder = colors.window.urgent.border.window;
  };
  placeholder = {
    border = colors.window.border.tabs;
    inherit (colors.window.placeholder') background;
    inherit (colors.window.placeholder') text;
    inherit (colors.window.placeholder') indicator;
    childBorder = colors.window.placeholder'.border;
  };
}
