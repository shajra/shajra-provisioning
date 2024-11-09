pkgs: pkgs-unstable: colors:

let

    window-created = pkgs.writers.writeDash "yabai-window-created" ''
        CURRENT_ID="$(yabai -m query --windows --window | "${pkgs.jq}/bin/jq" '.id')"
        if ! [ "$CURRENT_ID" = "$YABAI_WINDOW_ID" ]
        then yabai -m window --focus "$YABAI_WINDOW_ID"
        fi
    '';
    window-destroyed = pkgs.writers.writeDash "yabai-window-destroyed" ''
        yabai -m query --windows --window || yabai -m window --focus first
    '';

in {
    enable = true;
    package = pkgs-unstable.yabai;
    enableScriptingAddition = true;
    config = {
        # global settings (in order from 'man yabai')
        external_bar              = "all:35:0";
        mouse_follows_focus       = "off";
        focus_follows_mouse       = "off";
        window_origin_display     = "focused";
        window_placement          = "second_child";
        window_zoom_persist       = "on";
        window_shadow             = "float";
        window_opacity            = "on";
        window_opacity_duration   = 0.2;
        active_window_opacity     = 1.0;
        normal_window_opacity     = 0.975;
        window_animation_duration = 0.3;
        insert_feedback_color     = colors.semantic.unifying;
        split_ratio               = 0.50;
        split_type                = "auto";
        auto_balance              = "off";
        mouse_modifier            = "fn";
        mouse_action1             = "move";
        mouse_action2             = "resize";
        mouse_drop_action         = "swap";

        # general space settings
        layout                    = "bsp";
        top_padding               = 14;
        bottom_padding            = 8;
        left_padding              = 8;
        right_padding             = 8;
        window_gap                = 12;
    };
    extraConfig = ''
        # DESIGN: https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release)#macos-big-sur---automatically-load-scripting-addition-on-startup
        yabai -m signal --add \
            event=dock_did_restart \
            action="sudo yabai --load-sa" \
            label=dock_did_restart
        yabai -m signal --add \
            event=window_focused \
            action="${pkgs-unstable.sketchybar-window-focus}/bin/sketchybar-window-focus" \
            label=window_focused
        yabai -m signal --add \
            event=window_created \
            action="${window-created}" \
            label=window_created
        yabai -m signal --add \
            event=window_destroyed \
            action="${window-destroyed}" \
            label=window_destroyed
        yabai -m signal --add \
            event=display_removed \
            action="sketchybar --trigger display_removed" \
            label=display_removed
        yabai -m signal --add \
            event=display_added \
            action="sketchybar --trigger display_added" \
            label=display_added

        # custom rules
        #yabai -m rule --add app=Emacs title='^.+$' manage=on
        yabai -m rule --add app="emacs" role="AXTextField" subrole="AXStandardWindow" manage=on
        yabai -m rule --add app="Emacs" role="AXTextField" subrole="AXStandardWindow" manage=on
    '';
}
