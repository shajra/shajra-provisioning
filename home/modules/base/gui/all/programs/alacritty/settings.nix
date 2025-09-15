config: pkgs:

let

  format = f: x: pkgs.lib.colors.format "0x%R%G%B" (f x);
  id = x: x;
  lighten = pkgs.lib.colors.lightenByDec 32;
  inherit (config.theme.colors.nominal) foregroundFor;
  colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
  lightColors = pkgs.lib.colors.transformColors (format lighten) config.theme.colors;
  foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

in

{
  env.TERM = "xterm-256color";
  scrolling.history = 10000;

  font = {
    normal = {
      family = config.theme.fonts.monospaced.code.name;
      style = "Regular";
    };
    bold = {
      family = config.theme.fonts.monospaced.code.name;
      style = "Bold";
    };
    italic = {
      family = config.theme.fonts.monospaced.code.name;
      style = "Italic";
    };
  };

  colors = {
    draw_bold_text_with_bright_colors = false;
    primary = {
      inherit (colors.semantic) background;
      inherit (colors.semantic) foreground;
    };
    cursor = {
      cursor = colors.semantic.unifying;
      text = foreground.semantic.unifying;
    };
    selection = {
      background = colors.semantic.highlight;
      text = foreground.semantic.highlight;
    };
    search = {
      matches = {
        background = colors.semantic.background_highlighted;
        inherit (colors.semantic) foreground;
      };
      focused_match = {
        background = colors.semantic.foreground;
        foreground = colors.semantic.background;
      };
    };
    hints = {
      start = {
        background = colors.semantic.highlight;
        foreground = foreground.semantic.highlight;
      };
      end = {
        background = lightColors.semantic.highlight;
        foreground = foreground.semantic.highlight;
      };
    };
    normal = with colors.terminal.normal; {
      inherit
        black
        red
        green
        yellow
        blue
        magenta
        cyan
        white
        ;
    };
    bright = with colors.terminal.bright; {
      inherit
        black
        red
        green
        yellow
        blue
        magenta
        cyan
        white
        ;
    };
  };

  bell.color = colors.semantic.urgent;
}
