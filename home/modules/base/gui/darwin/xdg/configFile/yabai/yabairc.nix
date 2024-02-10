pkgs: colors:

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

in ''
#!/usr/bin/env sh

# initialization
# DESIGN: https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release)#macos-big-sur---automatically-load-scripting-addition-on-startup
sudo yabai --load-sa
yabai -m signal --add \
    event=dock_did_restart \
    action="sudo yabai --load-sa" \
    label=dock_did_restart
yabai -m signal --add \
    event=window_focused \
    action="${pkgs.sketchybar-window-focus}/bin/sketchybar-window-focus" \
    label=window_focused
yabai -m signal --add \
    event=window_created \
    action="${window-created}" \
    label=window_created
yabai -m signal --add \
    event=window_destroyed \
    action="${window-destroyed}" \
    label=window_destroyed

# global settings (in order from 'man yabai')
yabai -m config external_bar              all:35:0
yabai -m config mouse_follows_focus       off
yabai -m config focus_follows_mouse       off
yabai -m config window_origin_display     focused
yabai -m config window_placement          second_child
yabai -m config window_zoom_persist       on
yabai -m config window_shadow             float
yabai -m config window_opacity            on
yabai -m config window_opacity_duration   0.2
yabai -m config active_window_opacity     1.0
yabai -m config normal_window_opacity     0.975
yabai -m config window_animation_duration 0.3
yabai -m config insert_feedback_color     {colors.semantic.unifying}
yabai -m config split_ratio               0.50
yabai -m config split_type                auto
yabai -m config auto_balance              off
yabai -m config mouse_modifier            fn
yabai -m config mouse_action1             move
yabai -m config mouse_action2             resize
yabai -m config mouse_drop_action         swap

# general space settings
yabai -m config layout                    bsp
yabai -m config top_padding               14
yabai -m config bottom_padding            8
yabai -m config left_padding              8
yabai -m config right_padding             8
yabai -m config window_gap                12

# custom rules
#yabai -m rule --add app=Emacs title='^.+$' manage=on
yabai -m rule --add app="Emacs" role="AXTextField" subrole="AXStandardWindow" manage=on

echo "yabai configuration loaded..."
''
