config: pkgs: colors:

let

    jq    = "${pkgs.jq}/bin/jq";
    kitty = "${config.programs.kitty.package}/bin/kitty";

    space-move-script = pkgs.writeShellScriptBin "space-move" ''
        INDEX="$(yabai -m query --spaces --space | "${jq}" '.index')"
        yabai -m space --move "$1" || {
            case "$1" in
                next)
                    yabai -m space --display next \
                    && yabai -m space --move "$((INDEX))"
                    ;;
                prev)
                    yabai -m space --display prev \
                    && yabai -m space --move "$((INDEX))"
                    ;;
            esac
        }
        sketchybar --trigger space_change
        sketchybar --trigger space_windows_change
    '';
    space-move = "${space-move-script}/bin/space-move";

    space-create-script = pkgs.writeShellScriptBin "space-create" ''
        set -e
        set -o pipefail
        CURRENT="$(yabai -m query --spaces --space | "${jq}" '.index')"
        LAST="$(yabai -m query --spaces --display | "${jq}" 'map(.index)|max')"
        yabai -m space --create
        case "$1" in
            next) yabai -m space "$((LAST + 1))" --move "$((CURRENT + 1))" ;;
            prev) yabai -m space "$((LAST + 1))" --move "$CURRENT"          ;;
        esac
        sketchybar --trigger space_change
        sketchybar --trigger space_windows_change
    '';
    space-create = "${space-create-script}/bin/space-create";

    space-destroy-script = pkgs.writeShellScriptBin "space-destroy" ''
        set -e
        set -o pipefail
        INDEX="$(yabai -m query --spaces --space | "${jq}" '.index')"
        yabai -m space --destroy
        if [ "$INDEX" -gt 2 ]
        then
            yabai -m space --focus $((INDEX - 1))
            "${window-focus}"
        fi
    '';
    space-destroy = "${space-destroy-script}/bin/space-destroy";

    window-focus = "${pkgs.sketchybar-window-focus}/bin/sketchybar-window-focus";

    window-cycle-script = pkgs.writeShellScriptBin "window-cycle" ''
        yabai -m window --focus "$( \
            yabai -m query --windows --space \
            | "${jq}" -re "
                sort_by(.frame.x, .frame.y, .id)
                | map(select(.\"is-visible\"))
                | $1
                | if map(select(.\"has-focus\")) | length > 0
                  then nth(index(map(select(.\"has-focus\"))) - 1)
                  else .[0]
                  end
                | .id")"
        "${window-focus}"
    '';
    window-cycle = "${window-cycle-script}/bin/window-cycle";

    gaps-toggle-script = pkgs.writeShellScriptBin "gaps-toggle" ''
        GAP_BIG=24
        GAP_SLIM=12
        GAP_SIZE="$(yabai -m config window_gap)"
        if ! [ "$GAP_SIZE" -eq "$GAP_BIG" ]
        then GAP_SIZE="$GAP_BIG"
        else GAP_SIZE="$GAP_SLIM"
        fi
        yabai -m config                \
            window_gap     "$GAP_SIZE" \
               top_padding "$GAP_SIZE" \
            bottom_padding "$GAP_SIZE" \
              left_padding "$GAP_SIZE" \
             right_padding "$GAP_SIZE" \
        && sketchybar --bar margin="$((GAP_SIZE - 4))"
    '';
    gaps-toggle = "${gaps-toggle-script}/bin/gaps-toggle";

in ''
# Strategy for keybindings:
#
# - 'lcmd' is used consistently for all window management.
# - 'ralt' is unused for keybindings outside of skhd.
# - Where possible 'shift' implies moving of a window or space.
# - Where possible 'alt' indicates the target is a space.
# - 'ctrl' indicates stacked windows are involved.
# - 'h/j/k/l' is used consistently for west/south/north/east direction.
# - ',/.' (think angle brackets) are used for next/prev.
# - Digits are used for uniquely identifying spaces/displays.
#
# NOTE: some keys are mysterious codes.  Use "skhd -o" to discover what these
# codes should be

# define modes
:: default    : \
    borders \
        active_color=${colors.window.selected.focused.border.window} ; \
    sketchybar --bar border_color=${colors.semantic.unifying}
:: passthru   : sketchybar --bar border_color=${colors.semantic.warning}
:: size     @ : borders active_color=${colors.semantic.info}

# mode: returning home
size, passthru < lcmd + shift - escape ; default
size, passthru < lcmd         - escape ; default
size           <                escape ; default
size           <                return ; default

# mode: resizing, with toggle back
lcmd        - s ; size
size < lcmd - s ; default

# mode: passthrough, with toggle back
lcmd            - escape ; passthru
passthru < lcmd - escape ; default

# focus window
lcmd - h : yabai -m window  --focus west  || yabai -m display --focus west
lcmd - j : yabai -m window  --focus south || yabai -m display --focus south
lcmd - k : yabai -m window  --focus north || yabai -m display --focus north
lcmd - l : yabai -m window  --focus east  || yabai -m display --focus east

# warp window, or move if floating
lcmd + shift - h : yabai -m window --warp west  || $(yabai -m window --display west  && yabai -m display --focus west  && yabai -m window --warp last)  || yabai -m window --move rel:-20:0 ; "${window-focus}"
lcmd + shift - j : yabai -m window --warp south || $(yabai -m window --display south && yabai -m display --focus south)                                 || yabai -m window --move rel:0:20  ; "${window-focus}"
lcmd + shift - k : yabai -m window --warp north || $(yabai -m window --display north && yabai -m display --focus north)                                 || yabai -m window --move rel:0:-20 ; "${window-focus}"
lcmd + shift - l : yabai -m window --warp east  || $(yabai -m window --display east  && yabai -m display --focus east  && yabai -m window --warp first) || yabai -m window --move rel:20:0  ; "${window-focus}"

# swap window or fit to grid if floating
lcmd + shift + alt - h : yabai -m window --swap west  || yabai -m window --grid 1:2:0:0:1:1 ; "${window-focus}"
lcmd + shift + alt - j : yabai -m window --swap south                                       ; "${window-focus}"
lcmd + shift + alt - k : yabai -m window --swap north || yabai -m window --grid 1:1:0:0:1:1 ; "${window-focus}"
lcmd + shift + alt - l : yabai -m window --swap east  || yabai -m window --grid 1:2:1:0:1:1 ; "${window-focus}"

# stack window
lcmd + shift + ctrl - h : yabai -m window  west --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lcmd + shift + ctrl - j : yabai -m window south --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lcmd + shift + ctrl - k : yabai -m window north --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lcmd + shift + ctrl - l : yabai -m window  east --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"

# set insertion point in focused container
lcmd + alt - i : yabai -m window --insert cancel
lcmd + alt - h : yabai -m window --insert west
lcmd + alt - j : yabai -m window --insert south
lcmd + alt - k : yabai -m window --insert north
lcmd + alt - l : yabai -m window --insert east
lcmd + alt - s : yabai -m window --insert stack

# increase window size
size < lcmd - h : yabai -m window --resize left:-20:0
size < lcmd - j : yabai -m window --resize bottom:0:20
size < lcmd - k : yabai -m window --resize top:0:-20
size < lcmd - l : yabai -m window --resize right:20:0
size <        h : yabai -m window --resize left:-20:0
size <        j : yabai -m window --resize bottom:0:20
size <        k : yabai -m window --resize top:0:-20
size <        l : yabai -m window --resize right:20:0

# decrease window size
size < lcmd + shift - h : yabai -m window --resize left:20:0
size < lcmd + shift - j : yabai -m window --resize bottom:0:-20
size < lcmd + shift - k : yabai -m window --resize top:0:20
size < lcmd + shift - l : yabai -m window --resize right:-20:0
size <        shift - h : yabai -m window --resize left:20:0
size <        shift - j : yabai -m window --resize bottom:0:-20
size <        shift - k : yabai -m window --resize top:0:20
size <        shift - l : yabai -m window --resize right:-20:0

# focus space
lcmd - 1    : yabai -m space --focus 1    ; "${window-focus}"
lcmd - 2    : yabai -m space --focus 2    ; "${window-focus}"
lcmd - 3    : yabai -m space --focus 3    ; "${window-focus}"
lcmd - 4    : yabai -m space --focus 4    ; "${window-focus}"
lcmd - 5    : yabai -m space --focus 5    ; "${window-focus}"
lcmd - 6    : yabai -m space --focus 6    ; "${window-focus}"
lcmd - 7    : yabai -m space --focus 7    ; "${window-focus}"
lcmd - 8    : yabai -m space --focus 8    ; "${window-focus}"
lcmd - 9    : yabai -m space --focus 9    ; "${window-focus}"
lcmd - 0x2C : yabai -m space --focus last ; "${window-focus}"
lcmd - 0x2B : yabai -m space --focus prev ; "${window-focus}"
lcmd - 0x2F : yabai -m space --focus next ; "${window-focus}"

# move window to space
lcmd + shift - 1    : yabai -m window --space 1
lcmd + shift - 2    : yabai -m window --space 2
lcmd + shift - 3    : yabai -m window --space 3
lcmd + shift - 4    : yabai -m window --space 4
lcmd + shift - 5    : yabai -m window --space 5
lcmd + shift - 6    : yabai -m window --space 6
lcmd + shift - 7    : yabai -m window --space 7
lcmd + shift - 8    : yabai -m window --space 8
lcmd + shift - 9    : yabai -m window --space 9
lcmd + shift - 0x2C : yabai -m window --space last
lcmd + shift - 0x2B : yabai -m window --space prev
lcmd + shift - 0x2F : yabai -m window --space next

# move space
lcmd + shift + alt - 1    : "${space-move}" 1
lcmd + shift + alt - 2    : "${space-move}" 2
lcmd + shift + alt - 3    : "${space-move}" 3
lcmd + shift + alt - 4    : "${space-move}" 4
lcmd + shift + alt - 5    : "${space-move}" 5
lcmd + shift + alt - 6    : "${space-move}" 6
lcmd + shift + alt - 7    : "${space-move}" 7
lcmd + shift + alt - 8    : "${space-move}" 8
lcmd + shift + alt - 9    : "${space-move}" 9
lcmd + shift + alt - 0x2C : "${space-move}" last
lcmd + shift + alt - 0x2B : "${space-move}" prev
lcmd + shift + alt - 0x2F : "${space-move}" next

# focus window within stack
lcmd + ctrl - 0x2B : yabai -m window --focus stack.prev
lcmd + ctrl - 0x2F : yabai -m window --focus stack.next

# move window within stack
lcmd + shift + ctrl - 0x2B : yabai -m window --swap stack.prev && yabai -m window --focus stack.prev
lcmd + shift + ctrl - 0x2F : yabai -m window --swap stack.next && yabai -m window --focus stack.next

# create spaces
lcmd + alt - 0x2C : "${space-create}" last
lcmd + alt - 0x2B : "${space-create}" prev
lcmd + alt - 0x2F : "${space-create}" next

# open terminal
lcmd - return : "${kitty}" --single-instance --wait-for-single-instance-window-close --directory ~

# close window
lcmd - q : yabai -m window --close

# destroy space
lcmd + alt - q : "${space-destroy}"

# cycle windows
lcmd         - tab : "${window-cycle}" reverse
lcmd + shift - tab : "${window-cycle}" '.'

# balance size of windows (0x18 = equals)
lcmd        - 0x18 : yabai -m space --balance
size < lcmd - 0x18 : yabai -m space --balance

# transpose split type
lcmd - t : yabai -m window --toggle split

# rotate tree
lcmd         - r : yabai -m space --rotate 90
lcmd + shift - r : yabai -m space --rotate 270

# mirror tree y-axis
lcmd - y : yabai -m space --mirror y-axis

# mirror tree x-axis
lcmd - x : yabai -m space --mirror x-axis

# toggle window parent zoom
lcmd - m : yabai -m window --toggle zoom-parent; "${window-focus}"

# toggle window fullscreen zoom
lcmd - f : yabai -m window --toggle zoom-fullscreen; "${window-focus}"

# toggle window native fullscreen
lcmd + shift - f : yabai -m window --toggle native-fullscreen; "${window-focus}"

# float / unfloat window and center on screen (0x1B = minus)
lcmd + shift - 0x1B : \
    yabai -m window --toggle float &&   \
    yabai -m window --grid 4:4:1:1:2:2; \
    "${window-focus}"

# toggle sticky
lcmd + shift - s : yabai -m window --toggle sticky

lcmd - 0x2A : "${gaps-toggle}"

# toggle sticky, float and resize to picture-in-picture size
lcmd - p : yabai -m window --toggle sticky && \
           yabai -m window --grid 5:5:4:0:1:1 \

# toggle mission control (application-level)
lcmd - a : yabai -m window --toggle expose

.blacklist [
    "VMware Fusion"
]
''
