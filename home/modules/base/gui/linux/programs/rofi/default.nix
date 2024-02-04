config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "#%R%G%B%A" (f x);
    id = x: x;
    addAlpha = pkgs.lib.colors.withAlphaDec 247;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    transparent = pkgs.lib.colors.transformColors (format addAlpha) config.theme.colors;

in

{
    enable = true;
    extraConfig = {
        # DESIGN: unset bindings with Control+j/k, otherwise a conflict
        dpi = 0;
        kb-accept-entry = "Control+m,Return,KP_Enter";
        kb-remove-to-eol = "";
        kb-row-down = "Down,Control+j";
        kb-row-up = "Up,Control+k";
    };
    font = "${config.theme.fonts.monospaced.code.name} 12";
    plugins = [
        pkgs.rofi-calc
        pkgs.rofi-emoji
    ];
    theme = import ./theme.nix {
        inherit (config.lib.formats.rasi) mkLiteral;
        theme_background    = colors.semantic.background;
        theme_background_hl = colors.semantic.background_highlighted;
        theme_background_tr = transparent.semantic.background;
        theme_foreground    = colors.semantic.foreground;
        theme_foreground_em = colors.semantic.foreground_emphasized;
        theme_unifying      = colors.semantic.unifying;
        theme_highlight     = colors.semantic.highlight;
        theme_urgent        = colors.semantic.urgent;
    };
}
