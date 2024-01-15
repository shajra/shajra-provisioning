config: pkgs:

let

    kitty = "${config.programs.kitty.package}/bin/kitty";
    jq    = "${pkgs.jq}/bin/jq";

    format = f: x: pkgs.lib.colors.format "0xff%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

in {
    "sketchybar".source = pkgs.runCommandNoCC "sketchybarrc" {} ''
        cp -r "${./sketchybar}" "$out"
        chmod -R +w "$out"
        substituteInPlace "$out/sketchybarrc" \
            --replace @LUA@ "${pkgs.lua5_4}" \
            --replace @SKETCHYBAR_LUA_SO@ "${pkgs.sketchybar-lua}"
        substituteInPlace "$out/colors.lua" \
            --replace @COLORS_UNIFYING@      "${colors.semantic.unifying}" \
            --replace @COLORS_PRIMARY_BG@    "${colors.semantic.background}" \
            --replace @COLORS_PRIMARY_FG@    "${colors.semantic.foreground}" \
            --replace @COLORS_SECONDARY_BG@  "${colors.semantic.background_highlighted}" \
            --replace @COLORS_SECONDARY_FG@  "${colors.semantic.foreground}" \
            --replace @COLORS_UNSELECTED_BG@ "${colors.window.unselected.background}" \
            --replace @COLORS_UNSELECTED_FG@ "${colors.window.unselected.text}" \
            --replace @COLORS_SELECTED_BG@   "${colors.window.selected.focused.background}" \
            --replace @COLORS_SELECTED_FG@   "${colors.window.selected.focused.text}"
        substituteInPlace "$out/settings.lua" \
            --replace @FONT@ "${config.theme.fonts.proportional.name}"
        {
            echo "return {"
            for f in "${pkgs.sources.sketchybar-font-src}/mappings/"*
            do cat $f | sed 's/ *| */\n/g' | while read -r s
                do echo "    [$s]" = \"''${f##*/}\",
                done
            done
            echo '    [".kitty-wrapped"] = ":terminal:",'
            echo "}"
        } > "$out/emojis.lua"
        chmod +x "$out/sketchybarrc"
    '';
    "skhd/skhdrc".text = import skhd/skhdrc.nix kitty jq colors;
    "yabai/yabairc".text = import yabai/yabairc.nix colors;
    "yabai/yabairc".executable = true;
    "borders/bordersrc".text = import borders/bordersrc.nix colors;
    "borders/bordersrc".executable = true;
}
