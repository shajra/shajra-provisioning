pkgs:

let
    hintsOpen = type: "kitten hints --type=${type}"
        + " --hints-text-color 'green' --hints-foreground-color white";
    hintsInsert = type: "${hintsOpen type} --program -";
in

{
    enable = true;
    extraConfig = ''
        font_size 11
        scrollback_pager nvim -u NORC -c 'set ft=man' -c 'hi Search ctermbg=LightGrey' -
    '';
    font.name = "SauceCodePro Nerd Font Mono";
    font.package = pkgs.nerdfonts;
    keybindings = {
        "ctrl+shift+e"         = hintsOpen   "url";
        "ctrl+shift+p>f"       = hintsInsert "path";
        "ctrl+shift+p>shift+f" = hintsOpen   "path";
        "ctrl+shift+p>h"       = hintsInsert "hash";
        "ctrl+shift+p>l"       = hintsInsert "line";
        "ctrl+shift+p>n"       = hintsOpen   "linenum";
        "ctrl+shift+p>w"       = hintsInsert "word";
        "ctrl+shift+p>y"       = hintsOpen   "hyperlink";
    };
    settings = {
        enable_audio_bell    = false;
        background           = "#fdf6e3";
        foreground           = "#657b83";
        cursor               = "#586e75";
        cursor_text_color    = "background";
        selection_background = "#475b62";
        selection_foreground = "#eae3cb";
        color0               = "#073642";
        color8               = "#002b36";
        color1               = "#dc322f";
        color9               = "#cb4b16";
        color2               = "#859900";
        color10              = "#586e75";
        color3               = "#b58900";
        color11              = "#657b83";
        color4               = "#268bd2";
        color12              = "#839496";
        color5               = "#d33682";
        color13              = "#6c71c4";
        color6               = "#2aa198";
        color14              = "#93a1a1";
        color7               = "#eee8d5";
        color15              = "#fdf6e3";
        url_color            = "#2aa198";
        mark1_foreground     = "#fdf6e3";
        mark1_background     = "#859900";
        mark2_foreground     = "#fdf6e3";
        mark2_background     = "#268bd2";
        mark3_foreground     = "#fdf6e3";
        mark3_background     = "#cb4b16";
        tab_bar_style        = "powerline";
    };
}
