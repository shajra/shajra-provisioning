config: pkgs:

let

    format = f: x: pkgs.lib.colors.format "0xff%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

    sketchybar.template = pkgs.substituteAllFiles {
        src = ./sketchybar;
        files = [
            "colors.lua"
            "settings.lua"
            "sketchybarrc"
        ];
        lua                  = pkgs.lua5_4;
        sketchybar_lua_so    = pkgs.sketchybar-lua;
        colors_unifying      = colors.semantic.unifying;
        colors_info          = colors.semantic.info;
        colors_warning       = colors.semantic.warning;
        colors_urgent        = colors.semantic.urgent;
        colors_primary_bg    = colors.semantic.background;
        colors_primary_fg    = colors.semantic.foreground;
        colors_secondary_bg  = colors.semantic.background_highlighted;
        colors_secondary_fg  = colors.semantic.foreground;
        colors_unselected_bg = colors.window.unselected.background;
        colors_unselected_fg = colors.window.unselected.text;
        colors_selected_bg   = colors.window.selected.focused.background;
        colors_selected_fg   = colors.window.selected.focused.text;
        font_family          = config.theme.fonts.proportional.name;
    };

    sketchybar.emojis = pkgs.runCommand "sketchybar-emojis" {} ''
        mkdir -p "$out"
        {
            echo "return {"
            for f in "${pkgs.sources.sketchybar-font-src}/mappings/"*
            do cat $f | sed 's/ *| */\n/g' | while read -r s
                do echo "    [$s]" = \"''${f##*/}\",
                done
            done
            echo '    [".kitty-wrapped"] = ":terminal:",'
            echo '    ["Google Chrome Beta"] = ":google_chrome:",'
            echo '    ["Microsoft Edge Beta"] = ":microsoft_edge:",'
            echo "}"
        } > "$out/emojis.lua"
    '';

    sketchybar.rc = pkgs.runCommand "sketchybar-rc" {} ''
        mkdir -p "$out"
        cp ${sketchybar.template}/sketchybarrc "$out"
        chmod +x "$out/sketchybarrc"
    '';

in {
    "sketchybar".source = pkgs.symlinkJoin {
        name = "sketchybar";
        paths = with sketchybar; [ rc template emojis ./sketchybar ];
    };
    "skhd/skhdrc".text = import skhd/skhdrc.nix config pkgs colors;
    "yabai/yabairc".text = import yabai/yabairc.nix pkgs colors;
    "yabai/yabairc".executable = true;
    "borders/bordersrc".text = import borders/bordersrc.nix colors;
    "borders/bordersrc".executable = true;
}
