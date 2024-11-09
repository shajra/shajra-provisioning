config: pkgs:

let

    format = pkgs.lib.colors.format "%R%G%B";
    colors = pkgs.lib.colors.transformColors format config.theme.colors;
    colorName = c: pkgs.lib.colors.colorName c config.theme.colors.terminal;
    doom.template = pkgs.substituteAllFiles {
        src = ./doom;
        files = ["config.el"];
        theme_font_mono_code    = config.theme.fonts.monospaced.code.name;
        theme_font_mono_serif   = config.theme.fonts.monospaced.serif.name;
        theme_font_proportional = config.theme.fonts.proportional.name;
        theme_doom_name         = config.theme.external.doom.name;
        theme_color_unifying    = colorName config.theme.colors.semantic.unifying;
        theme_color_normal_black = colors.terminal.normal.black;
        theme_color_normal_white = colors.terminal.normal.white;
        theme_color_bright_black = colors.terminal.bright.black;
        theme_color_bright_white = colors.terminal.bright.white;
    };

in

{
    # DESIGN: rsync over symlinkJoin because symlinks disrupt Emacs autoloading
    doom.source = pkgs.runCommand "doom-source" {
        nativeBuildInputs = [ pkgs.rsync ];
    } ''rsync --archive "${doom.template}/" "${./doom}/" "$out"'';

    "fish/completions/devour.fish".source = fish/completions/devour.fish;
    "fish/completions/jj.fish".source = "${pkgs.sources.jj-fish}/jj.fish";
    "fish/conf.d/direnv.fish".text =
        pkgs.callPackage fish/direnv.nix { inherit (config.xdg) cacheHome; };
    "fish/set-universal.fish".onChange = import fish/onChange.nix config;
    "fish/set-universal.fish".source = pkgs.substituteAll {
        src = fish/set-universal.fish;
        preview_file = "${pkgs.preview-file}/bin/preview-file";
        theme_background        = colors.semantic.background;
        theme_background_hl     = colors.semantic.background_highlighted;
        theme_foreground_sh     = colors.semantic.foreground_shadowed;
        theme_inv_foreground    = colors.semantic.inverse.foreground;
        theme_foreground        = colors.semantic.foreground;
        theme_foreground_em     = colors.semantic.foreground_emphasized;
        theme_inv_background_hl = colors.semantic.inverse.background_highlighted;
        theme_inv_background    = colors.semantic.inverse.background;
        theme_blue    = colors.nominal.blue;
        theme_cyan    = colors.nominal.cyan;
        theme_green   = colors.nominal.green;
        theme_magenta = colors.nominal.magenta;
        theme_red     = colors.nominal.red;
        theme_violet  = colors.nominal.violet;
        theme_yellow  = colors.nominal.yellow;
    };
    macchina.source = ./macchina;
}
