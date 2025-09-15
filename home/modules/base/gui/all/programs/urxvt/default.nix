config: pkgs:

{
  enable = true;

  # REVISIT: home-manager pointing to obsolete attribute name
  package = pkgs.rxvt-unicode-unwrapped;

  fonts = [
    "xft:${config.theme.fonts.monospaced.code.name}:size=11,xft:Symbola"
  ];
  extraConfig = {
    "perl-ext-common" = "default,font-size";
    "perl-lib" = "${pkgs.rxvt-unicode-plugins.font-size}/lib/urxvt/perl";
  };
  keybindings = {
    "Shift-C-C" = "eval:selection_to_clipboard";
    "Shift-C-V" = "eval:paste_clipboard";
    "Shift-C-underscore" = "font-size:decrease";
    "Shift-C-plus" = "font-size:increase";
    "Shift-C-M-underscore" = "font-size:decglobal";
    "Shift-C-M-plus" = "font-size:incglobal";
    "Shift-C-question" = "font-size:show";
  };

}
