config: pkgs:

let

  format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
  id = x: x;
  inherit (config.theme.colors.nominal) foregroundFor;
  colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
  foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

in

{
  enable = true;
  bars = {
    bottom = {
      icons = "awesome6";
      settings.theme = {
        overrides = {
          idle_bg = colors.semantic.unifying;
          idle_fg = foreground.semantic.unifying;
          good_bg = colors.semantic.good;
          good_fg = foreground.semantic.good;
          info_bg = colors.semantic.info;
          info_fg = foreground.semantic.info;
          warning_bg = colors.semantic.warning;
          warning_fg = foreground.semantic.warning;
          critical_bg = colors.semantic.urgent;
          critical_fg = foreground.semantic.urgent;

          alternating_tint_bg = "#202020"; # lighten
          alternating_tint_fg = "#202020"; # lighten

          separator_bg = "auto";
          separator_fg = "auto";
          separator = "";
          #separator           = "";
        };
      };
    };
  };
}
