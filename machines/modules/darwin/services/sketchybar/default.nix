config: pkgs: pkgs-unstable: colors:

let

    template = pkgs.replaceVarsAllFiles {
        src = ./config;
        files = [
            "colors.lua"
            "settings.lua"
            "items/cpu.lua"
            "items/spaces.lua"
        ];
    } {
        grep                = "${pkgs.gnugrep}/bin/grep";
        sketchybar_cpu      = "${pkgs-unstable.sketchybar-helpers}/bin/sketchybar-cpu";
        timeout             = "${pkgs.coreutils}/bin/timeout";
        colors_blue         = colors.nominal.blue;
        colors_red          = colors.nominal.red;
        colors_unifying     = colors.semantic.unifying;
        colors_good         = colors.semantic.good;
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

    appIconNames = pkgs.runCommand "sketchybar-app-icon-names" {} ''
        mkdir -p "$out"
        {
            echo "return {"
            "${pkgs.findutils}/bin/find" \
                "${pkgs-unstable.sources.sketchybar-font-src}/mappings" \
                -type f \
                -mindepth 1 \
                -maxdepth 1 | while read -r f
            do cat $f | sed 's/ *| */\n/g;s/[*]//g' | while read -r s
                do echo "    [$s]" = \"''${f##*/}\",
                done
            done
            echo '    [".kitty-wrapped"] = ":kitty:",'
            echo '    ["iTerm2"] = ":iterm:",'
            echo '    ["Google Chrome Beta"] = ":google_chrome:",'
            echo '    ["Microsoft Edge Beta"] = ":microsoft_edge:",'
            echo "}"
        } > "$out/app_icon_names.lua"
    '';

    combined = pkgs.symlinkJoin {
        name = "sketchybar-config";
        paths = [ template appIconNames ./config ];
    };

    luaposix = pkgs-unstable.lua5_4.pkgs.buildLuarocksPackage {
        pname = "luaposix";
        version = "36.3-1";
        src = pkgs-unstable.sources.luaposix;
    };

    lua = pkgs-unstable.lua5_4.withPackages (ps: [
        ps.lua-cjson
        luaposix
    ]);

in {
    enable = true;
    package = pkgs-unstable.sketchybar;
    config = ''
        #!${lua}/bin/lua

        package.cpath = package.cpath .. ";${pkgs-unstable.sketchybar-lua}/?.so"
        package.path = package.path .. ";${combined}/?.lua;${combined}/?/init.lua"

        local Aerospace = require("aerospace")
        local aerospace = Aerospace.new()
        while not aerospace:is_initialized() do
            os.execute("sleep 0.1")
        end
        sbar = require("sketchybar")
        sbar.aerospace = aerospace
        sbar.begin_config()
        require("init")
        sbar.hotload(false)
        sbar.end_config()
        sbar.event_loop()
    '';
}
