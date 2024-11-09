config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    inherit (config.theme.colors.nominal) foregroundFor;
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

    scrollback.mouseMap =
        "mouse_map kitty_mod+right press ungrabbed combine"
        + " : mouse_select_command_output"
        + " : kitty_scrollback_nvim --config ksb_builtin_last_visited_cmd_output";

    scrollback.actionAlias = "kitty_scrollback_nvim kitten"
        + " ${pkgs.sources.kitty-scrollback-nvim}/python/kitty_scrollback_nvim.py";

in

{
    enable = true;
    extraConfig = ''
        ${scrollback.mouseMap}
    '';
    font = config.theme.fonts.monospaced.code;
    keybindings = {
        "kitty_mod+e"         = hintsOpen   "url";
        "kitty_mod+g"         = "kitty_scrollback_nvim --config ksb_builtin_last_cmd_output";
        "kitty_mod+h"         = "kitty_scrollback_nvim";
        "kitty_mod+p>f"       = hintsInsert "path";
        "kitty_mod+p>h"       = hintsInsert "hash";
        "kitty_mod+p>l"       = hintsInsert "line";
        "kitty_mod+p>n"       = hintsOpen   "linenum";
        "kitty_mod+p>shift+f" = hintsOpen   "path";
        "kitty_mod+p>w"       = hintsInsert "word";
        "kitty_mod+p>y"       = hintsOpen   "hyperlink";
    };
    settings = {
        allow_remote_control = true;
        listen_on = "unix:/tmp/kitty";
        shell_integration = "enabled";
        action_alias = scrollback.actionAlias;

        inherit (colors.semantic) background;
        inherit (colors.semantic) foreground;

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
