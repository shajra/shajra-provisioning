config: pkgs:

let
    display-count = pkgs.writers.writeDash "yabai-display-count" ''
        yabai -m query --displays | ${pkgs.jq}/bin/jq length
    '';

    session-save = pkgs.writers.writeDash "yabai-session-save" ''
        yabai -m query --windows | ${pkgs.jq}/bin/jq -re '
            . as $top
            | max_by(.space) | .space as $max_space
            | "${pkgs.coreutils}/bin/sleep 1"
            , "SPACES_CUR=$(yabai -m query --spaces | ${pkgs.jq}/bin/jq length)"
            , "for _i in $(${pkgs.coreutils}/bin/seq $((SPACES_CUR + 1)) \($max_space))"
            , "do yabai -m space --create last"
            , "done"
            , ($top[]
            | select(.minimized != 1)
            | "yabai -m window \(.id) --space \(.space)"
        )'
    '';

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
            "items/spaces.lua"
        ];
        lua                = "${pkgs.lua5_4}/bin/lua";
        timeout            = "${pkgs.coreutils}/bin/timeout";
        ping               = "${pkgs.inetutils}/bin/ping";
        session_save       = "${session-save}";
        display_count      = "${display-count}";
        sketchybar_lua_so  = pkgs.sketchybar-lua;
        colors_blue         = colors.nominal.blue;
        colors_red          = colors.nominal.red;
        colors_unifying     = colors.semantic.unifying;
        colors_info         = colors.semantic.info;
        colors_warning      = colors.semantic.warning;
        colors_urgent       = colors.semantic.urgent;
        colors_primary_bg   = colors.semantic.background;
        colors_primary_fg   = colors.semantic.foreground;
        colors_secondary_bg = colors.semantic.background_highlighted;
        colors_secondary_fg = colors.semantic.foreground;
        colors_unselected_bg         = colors.window.unselected.background;
        colors_unselected_fg         = colors.window.unselected.text;
        colors_selected_focused_bg   = colors.window.selected.focused.background;
        colors_selected_focused_fg   = colors.window.selected.focused.text;
        colors_selected_unfocused_bg = colors.window.selected.unfocused.background;
        colors_selected_unfocused_fg = colors.window.selected.unfocused.text;
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
            echo '    [".kitty-wrapped"] = ":kitty:",'
            echo '    ["iTerm2"] = ":iterm:",'
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
