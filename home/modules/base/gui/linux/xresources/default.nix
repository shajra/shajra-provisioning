config: pkgs:

let

  format = pkgs.lib.colors.format "#%R%G%B";
  colors = pkgs.lib.colors.transformColors format config.theme.colors;

in

{
  extraConfig = ''
    #include "${config.home.homeDirectory}/.Xresources.dpi"
  '';
  properties = {
    "*color0" = colors.terminal.normal.black;
    "*color1" = colors.terminal.normal.red;
    "*color2" = colors.terminal.normal.green;
    "*color3" = colors.terminal.normal.yellow;
    "*color4" = colors.terminal.normal.blue;
    "*color5" = colors.terminal.normal.magenta;
    "*color6" = colors.terminal.normal.cyan;
    "*color7" = colors.terminal.normal.white;

    "*color8" = colors.terminal.bright.black;
    "*color9" = colors.terminal.bright.red;
    "*color10" = colors.terminal.bright.green;
    "*color11" = colors.terminal.bright.yellow;
    "*color12" = colors.terminal.bright.blue;
    "*color13" = colors.terminal.bright.magenta;
    "*color14" = colors.terminal.bright.cyan;
    "*color15" = colors.terminal.bright.white;

    "*background" = colors.semantic.background;
    "*foreground" = colors.semantic.foreground;
    "*fading" = 30;
    "*fadeColor" = colors.semantic.background;
    "*cursorColor" = colors.semantic.unifying;

    "Xcursor.theme" = "Adwaita";
    "Xcursor.size" = 48;
  };
}
