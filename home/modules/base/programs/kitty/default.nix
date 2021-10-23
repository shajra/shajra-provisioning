config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

    hintsBg = pkgs.lib.colors.colorName
        config.theme.colors.semantic.highlight
        config.theme.colors.terminal;

    hintsFg = pkgs.lib.colors.colorName
        (config.theme.colors.nominal.foregroundFor hintsBg)
        config.theme.colors.terminal;

    hintsOpen = type: "kitten hints --type=${type}"
        + " --hints-text-color ${hintsBg} --hints-foreground-color ${hintsFg}";

    hintsInsert = type: "${hintsOpen type} --program -";

    scrollbackNvim =
        '' "${config.programs.neovim.package}/bin/nvim" ''
        + '' -u NORC ''
        + '' -c "map q :qa!<CR>" ''
        + '' -c "terminal cat "<(cat)" - " ''
        + '' -c "map i <Nop>" ''
        + '' -c "set clipboard+=unnamedplus" ''
        + '' -c "call cursor(CURSOR_LINE, CURSOR_COLUMN)" '';

in

{
    enable = true;
    extraConfig = ''
        scrollback_pager "${pkgs.bash}/bin/bash" -c '${scrollbackNvim}'
    '';
    font = config.theme.fonts.monospaced.code;
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

        background              = colors.semantic.background;
        foreground              = colors.semantic.foreground;

        cursor                  = colors.semantic.unifying;
        cursor_text_color       = foreground.semantic.unifying;

        selection_background    = colors.semantic.highlight;
        selection_foreground    = foreground.semantic.highlight;

        url_color               = colors.nominal.cyan;

        mark1_foreground        = foreground.nominal.green;
        mark1_background        = colors.nominal.green;
        mark2_foreground        = foreground.nominal.blue;
        mark2_background        = colors.nominal.blue;
        mark3_foreground        = foreground.nominal.orange;
        mark3_background        = colors.nominal.orange;

        tab_bar_style           = "powerline";
        active_tab_background   = colors.window.selected.focused.background;
        active_tab_foreground   = colors.window.selected.focused.text;
        active_border_color     = colors.window.selected.focused.border.window;
        inactive_tab_background = colors.window.unselected.background;
        inactive_tab_foreground = colors.window.unselected.text;
        inactive_border_color   = colors.window.unselected.border.window;

        color0                  = colors.terminal.normal.black;
        color1                  = colors.terminal.normal.red;
        color2                  = colors.terminal.normal.green;
        color3                  = colors.terminal.normal.yellow;
        color4                  = colors.terminal.normal.blue;
        color5                  = colors.terminal.normal.magenta;
        color6                  = colors.terminal.normal.cyan;
        color7                  = colors.terminal.normal.white;

        color8                  = colors.terminal.bright.black;
        color9                  = colors.terminal.bright.red;
        color10                 = colors.terminal.bright.green;
        color11                 = colors.terminal.bright.yellow;
        color12                 = colors.terminal.bright.blue;
        color13                 = colors.terminal.bright.magenta;
        color14                 = colors.terminal.bright.cyan;
        color15                 = colors.terminal.bright.white;

        enable_audio_bell       = false;
        bell_border_color       = colors.semantic.urgent;

    };
}
