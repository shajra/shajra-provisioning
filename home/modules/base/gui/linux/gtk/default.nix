config:

let
  inherit (config.theme.external.gtk) colorScheme theme;
in

{
  enable = true;
  font = config.theme.fonts.proportional;
  inherit colorScheme;
  gtk3.theme = theme;
  gtk4.theme = theme;
}
