{ config, options, lib, pkgs, ... }:

let

    cfg = config.theme;

    optsOf = x: { inherit (x) options; };

    font.mkOpt = description: lib.mkOption {
        inherit description;
        type = lib.hm.types.fontType;
    };

    color.options = {
        red   = lib.mkOption { type = lib.types.ints.u8; };
        green = lib.mkOption { type = lib.types.ints.u8; };
        blue  = lib.mkOption { type = lib.types.ints.u8; };
        alpha = lib.mkOption {
            description = "0 fully transparent, 255 fully opaque.";
            type = lib.types.ints.u8;
        };
    };

    color.type = lib.types.submodule (optsOf color);

    color.mkOpt = description: lib.mkOption {
        inherit description;
        type = color.type;
    };

    colorFunc.mkOpt = description: lib.mkOption {
        inherit description;
        type = lib.types.functionTo color.type;
    };

    placeholder.options = {
        background = color.mkOpt "Background color for placeholder title bar.";
        text       = color.mkOpt "Text color for placeholder title bar.";
        border     = color.mkOpt "Border color for placeholder window (ignored by I3).";
        indicator  = color.mkOpt "Color for placeholder windowing indicator (ignored by I3).";
    };

    placeholder.type = lib.types.submodule (optsOf placeholder);

    placeholder.mkOpt = lib.mkOption {
        description = "When restoring a layout, colors for window not yet populated.";
        type = placeholder.type;
    };

    window.options = {
        background       = color.mkOpt "Background color for title bar or workspace button.";
        text             = color.mkOpt "Text color for title bar or workspace button.";
        border.window    = color.mkOpt "Border color for window.";
        border.workspace = color.mkOpt "Border color for workspace button.";
        indicator        = color.mkOpt "Color for indicator of windowing mode.";
    };

    window.type = lib.types.submodule (optsOf window);

    window.mkOpt = description: lib.mkOption {
        inherit description;
        type = window.type;
    };

    bindingMode.options = {
        background = color.mkOpt "Background color for binding mode button.";
        text       = color.mkOpt "Text color for binding mode button.";
        border     = color.mkOpt "Border color for binding mode button.";
    };

    bindingMode.type = lib.types.submodule (optsOf bindingMode);

    bindingMode.mkOpt = lib.mkOption {
        description = "Colors for binding mode button.";
        type = bindingMode.type;
    };

    terminal.options = {
        black   = color.mkOpt "Color for black in a terminal.";
        red     = color.mkOpt "Color for red in a terminal.";
        green   = color.mkOpt "Color for green in a terminal.";
        yellow  = color.mkOpt "Color for yellow in a terminal.";
        blue    = color.mkOpt "Color for blue in a terminal.";
        magenta = color.mkOpt "Color for magenta in a terminal.";
        cyan    = color.mkOpt "Color for cyan in a terminal.";
        white   = color.mkOpt "Color for white in a terminal.";
    };

    terminal.type = lib.types.submodule (optsOf terminal);

    terminal.mkOpt = description: lib.mkOption {
        inherit description;
        type = terminal.type;
    };

    theme.options = {
        fonts = {
            monospaced.code  = font.mkOpt "A fixed-width (non-proportional) font for coding.";
            monospaced.serif = font.mkOpt "A fixed-width (non-proportional) “typewriter” font with serifs.";
            proportional     = font.mkOpt "A variable-width font.";
        };
        colors = {
            nominal = {
                monotone = colorFunc.mkOpt "Function returning a monotone color given a brightness as an integer from 0 to 100.";
                foregroundFor = colorFunc.mkOpt "Function returning a monotone color given a background color.";
                blue = color.mkOpt "Color to use for blue.";
                cyan = color.mkOpt "Color to use for cyan.";
                green = color.mkOpt "Color to use for green.";
                magenta = color.mkOpt "Color to use for magenta.";
                orange = color.mkOpt "Color to use for orange.";
                red = color.mkOpt "Color to use for red.";
                violet = color.mkOpt "Color to use for violet.";
                yellow = color.mkOpt "Color to use for yellow.";
            };
            semantic = {
                background = color.mkOpt "Primary background color.";
                background_highlighted = color.mkOpt "Background highlight color.";
                foreground_shadowed = color.mkOpt "Color for dimmed foreground (for example, comments).";
                foreground = color.mkOpt "Primary foreground color.";
                foreground_emphasized = color.mkOpt "Emphasized foreground color";
                unifying = color.mkOpt "An accent color that ties the theme together.";
                highlight = color.mkOpt "An accent color for highlighting.";
                info = color.mkOpt "An accent color to indication a info status.";
                good = color.mkOpt "An accent color to indication a good status.";
                warning = color.mkOpt "An accent color to indication a warning.";
                urgent = color.mkOpt "An accent color to indication urgency.";
                inverse = {
                    background = color.mkOpt "Inverse background color.";
                    background_highlighted = color.mkOpt "Inverse background highlight color.";
                    foreground_shadowed = color.mkOpt "Inverse color for dimmed foreground (for example, comments).";
                    foreground = color.mkOpt "Inverse foreground color.";
                    foreground_emphasized = color.mkOpt "Inverse emphasized foreground color";
                };
            };
            window = {
                selected.focused   = window.mkOpt "Colors for a selected window or workspace that is also the current focus.";
                selected.unfocused = window.mkOpt "Colors for a selected window or workspace that is not the current focus.";
                unselected         = window.mkOpt "Colors for a unselected window or workspace.";
                urgent             = window.mkOpt "Colors for a window or workspace marked urgent.";
                placeholder        = placeholder.mkOpt;
                bindingMode        = bindingMode.mkOpt;
                border.tabs        = color.mkOpt "Color for 1-pixel border of tabs";
            };
            terminal.normal = terminal.mkOpt "Normal terminal colors.";
            terminal.bright = terminal.mkOpt "Bright terminal colors.";
        };
        external = {
            bat.name = lib.mkOption {
                description = "Name of theme for Bat.";
                type = lib.types.str;
            };
            bottom.name = lib.mkOption {
                description = "Name of theme for Bottom.";
                type = lib.types.str;
            };
            dircolors.extraConfig = options.programs.dircolors.extraConfig;
            doom.name = lib.mkOption {
                description = "Name of theme for Doom Emacs.";
                type = lib.types.str;
            };
            tridactyl = lib.mkOption {
                description = "";
                type = lib.types.submodule {
                    options = {
                        url = lib.mkOption {
                            description = "URL to Tridactyl theme CSS.";
                            type = lib.types.str;
                        };
                        name = lib.mkOption {
                            description = "Name of Tridactyl theme.";
                            type = lib.types.str;
                        };
                    };
                };
            };
            gtk = options.gtk.theme;
        };
    };

    theme.type = lib.types.submodule (optsOf theme);

    theme.mkOpt = lib.mkOption {
        description = "Theme options (for example, colors and fonts).";
        type = theme.type;
    };

    darkened = pkgs.lib.colors.darkenByDec 48;

    themes.solarized.light = {
        fonts = {
            monospaced.code = {
                name = "SauceCodePro Nerd Font Mono";
                package = pkgs.nerdfonts;
            };
            monospaced.serif = {
                name = "GoMono Nerd Font";
                package = pkgs.nerdfonts;
            };
            proportional = {
                name = "SourceSerif4";
                package = pkgs.source-serif;
            };
        };
        colors = with pkgs.lib.colors.palettes.solarized; rec {
            nominal = {
                inherit
                    monotone
                    foregroundFor
                    blue
                    cyan
                    green
                    magenta
                    orange
                    red
                    violet
                    yellow;
            };
            semantic = {
                inherit (light)
                    background
                    background_highlighted
                    foreground_shadowed
                    foreground
                    foreground_emphasized;
                unifying  = green;
                highlight = magenta;
                good      = green;
                info      = cyan;
                warning   = yellow;
                urgent    = magenta;
                inverse = {
                    inherit (dark)
                        background
                        background_highlighted
                        foreground_shadowed
                        foreground
                        foreground_emphasized;
                };
            };
            window = rec {
                selected.focused = {
                    background       = semantic.unifying;  # 60
                    text             = base3;              # 97
                    border.workspace = darkened selected.focused.background;
                    border.window    = selected.focused.background;
                    indicator        = darkened selected.focused.border.window;
                };
                selected.unfocused = {
                    background       = base1;   # 65
                    text             = base2;   # 92
                    border.workspace = base01;  # 20
                    border.window    = selected.unfocused.background;
                    indicator        = darkened selected.unfocused.border.window;
                };
                unselected = {
                    background       = base15;  # 78.5
                    text             = base00;  # 50
                    border.workspace = base00;  # 50
                    border.window    = unselected.background;
                    indicator        = darkened unselected.border.window;
                };
                urgent = {
                    background       = base15;           # 78.5
                    text             = semantic.urgent;  # 65
                    border.workspace = base1;            # 65
                    border.window    = urgent.background;
                    indicator        = darkened urgent.border.window;
                };
                placeholder = {
                    background = base3;   # 97
                    text       = base01;  # 45
                    border     = placeholder.background;  # ignored
                    indicator  = base2;   # 92, ignored
                };
                bindingMode = {
                    background = cyan;    # 60
                    text       = base3;   # 97
                    border     = base02;  # 20
                };
                border.tabs = base01;  # 45
            };
            terminal.normal = {
                black   = base02;
                red     = red;
                green   = green;
                yellow  = yellow;
                blue    = blue;
                magenta = magenta;
                cyan    = cyan;
                white   = base2;
            };
            terminal.bright = {
                black   = base03;
                red     = orange;
                green   = base01;
                yellow  = base00;
                blue    = base0;
                magenta = violet;
                cyan    = base1;
                white   = base3;
            };
        };
        external = {
            bat.name = "Solarized (light)";
            bottom.name = "gruvbox-light";
            dircolors.extraConfig = builtins.readFile
                "${pkgs.sources.dircolors-solarized}/dircolors.ansi-light";
            doom.name = "doom-solarized-light";
            gtk = {
                name = "NumixSolarizedLightMagenta";
                package = pkgs.numix-solarized-gtk-theme;
            };
            tridactyl = {
                url = "https://raw.githubusercontent.com/bezmi/"
                    + "base16-tridactyl/master/base16-solarized-light.css";
                name = "solarized-light";
            };
        };
    };

in {
    options.theme = theme.mkOpt;
    config.theme = themes.solarized.light;
}
