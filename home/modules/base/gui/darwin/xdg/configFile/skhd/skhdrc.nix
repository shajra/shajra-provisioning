config: pkgs: colors:

let

    jq    = "${pkgs.jq}/bin/jq";
    kitty = "${config.programs.kitty.package}/bin/kitty";

    space-move-script = pkgs.writeShellScriptBin "space-move" ''
        yabai -m space --move "$1"
        sketchybar --trigger space_change
        sketchybar --trigger space_windows_change
    '';
    space-move = "${space-move-script}/bin/space-move";

    space-create-script = pkgs.writeShellScriptBin "space-create" ''
        set -e
        set -o pipefail
        INDEX="$(yabai -m query --spaces --space | "${jq}" '.index')"
        yabai -m space --create
        case "$1" in
            next) yabai -m space last --move "$(($INDEX + 1))" ;;
            prev) yabai -m space last --move "$INDEX"          ;;
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
# - 'lalt' is used consistently for all window management.
# - 'ralt' is unused for keybindings outside of skhd.
# - Where possible 'shift' implies moving of a window or space.
# - Where possible 'cmd' indicates the target is a space.
# - 'ctrl' indicates stacked windows are involved.
# - 'h/j/k/l' is used consistently for west/south/north/east direction.
# - ',/.' (think angle brackets) are used for next/prev.
# - Digits are used for uniquely identifying spaces/displays.
#
# NOTE: some keys are mysterious codes.  Use "skhd -o" to discover what these
# codes should be.

# define modes
:: default    : \
    borders \
        active_color=${colors.window.selected.focused.border.window} ; \
    sketchybar --bar border_color=${colors.semantic.unifying}
:: passthru   : sketchybar --bar border_color=${colors.semantic.warning}
:: size     @ : borders active_color=${colors.semantic.info}

# mode: returning home
size, passthru < lalt + shift - escape ; default
size, passthru < lalt         - escape ; default
size           <                escape ; default
size           <                return ; default

# mode: resizing, with toggle back
lalt        - s ; size
size < lalt - s ; default

# mode: passthrough, with toggle back
lalt            - escape ; passthru
passthru < lalt - escape ; default

# open terminal
#lalt - return : alacritty # no args
lalt - return : "${kitty}" --single-instance --wait-for-single-instance-window-close --directory ~

# focus window
lalt - h : yabai -m window  --focus west  || yabai -m display --focus west
lalt - j : yabai -m window  --focus south || yabai -m display --focus south
lalt - k : yabai -m window  --focus north || yabai -m display --focus north
lalt - l : yabai -m window  --focus east  || yabai -m display --focus east

# swap window, or move if floating
lalt + shift - h : yabai -m window --swap west  || yabai -m window --move rel:-20:0
lalt + shift - j : yabai -m window --swap south || yabai -m window --move rel:0:20
lalt + shift - k : yabai -m window --swap north || yabai -m window --move rel:0:-20
lalt + shift - l : yabai -m window --swap east  || yabai -m window --move rel:20:0

# warp window or fit to grid if floating
lalt + shift + cmd - h : yabai -m window --warp west  || yabai -m window --grid 1:2:0:0:1:1 ; "${window-focus}"
lalt + shift + cmd - j : yabai -m window --warp south                                       ; "${window-focus}"
lalt + shift + cmd - k : yabai -m window --warp north || yabai -m window --grid 1:1:0:0:1:1 ; "${window-focus}"
lalt + shift + cmd - l : yabai -m window --warp east  || yabai -m window --grid 1:2:1:0:1:1 ; "${window-focus}"

# stack window
lalt + shift + ctrl - h : yabai -m window  west --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lalt + shift + ctrl - j : yabai -m window south --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lalt + shift + ctrl - k : yabai -m window north --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"
lalt + shift + ctrl - l : yabai -m window  east --stack $(yabai -m query --windows --window | jq -r '.id') ; "${window-focus}"

# set insertion point in focused container
lalt + cmd - i : yabai -m window --insert cancel
lalt + cmd - h : yabai -m window --insert west
lalt + cmd - j : yabai -m window --insert south
lalt + cmd - k : yabai -m window --insert north
lalt + cmd - l : yabai -m window --insert east
lalt + cmd - s : yabai -m window --insert stack

# increase window size
size < lalt - h : yabai -m window --resize left:-20:0
size < lalt - j : yabai -m window --resize bottom:0:20
size < lalt - k : yabai -m window --resize top:0:-20
size < lalt - l : yabai -m window --resize right:20:0

# decrease window size
size < lalt + shift - h : yabai -m window --resize left:20:0
size < lalt + shift - j : yabai -m window --resize bottom:0:-20
size < lalt + shift - k : yabai -m window --resize top:0:20
size < lalt + shift - l : yabai -m window --resize right:-20:0

# focus space
lalt - 0x2C : yabai -m space --focus last ; "${window-focus}"
lalt - 0x2B : yabai -m space --focus prev ; "${window-focus}"
lalt - 0x2F : yabai -m space --focus next ; "${window-focus}"
lalt - 1    : yabai -m space --focus 1    ; "${window-focus}"
lalt - 2    : yabai -m space --focus 2    ; "${window-focus}"
lalt - 3    : yabai -m space --focus 3    ; "${window-focus}"
lalt - 4    : yabai -m space --focus 4    ; "${window-focus}"
lalt - 5    : yabai -m space --focus 5    ; "${window-focus}"
lalt - 6    : yabai -m space --focus 6    ; "${window-focus}"
lalt - 7    : yabai -m space --focus 7    ; "${window-focus}"
lalt - 8    : yabai -m space --focus 8    ; "${window-focus}"
lalt - 9    : yabai -m space --focus 9    ; "${window-focus}"

# move window to space
lalt + shift - 0x2C : yabai -m window --space last
lalt + shift - 0x2B : yabai -m window --space prev
lalt + shift - 0x2F : yabai -m window --space next
lalt + shift - 1    : yabai -m window --space 1
lalt + shift - 2    : yabai -m window --space 2
lalt + shift - 3    : yabai -m window --space 3
lalt + shift - 4    : yabai -m window --space 4
lalt + shift - 5    : yabai -m window --space 5
lalt + shift - 6    : yabai -m window --space 6
lalt + shift - 7    : yabai -m window --space 7
lalt + shift - 8    : yabai -m window --space 8
lalt + shift - 9    : yabai -m window --space 9

# move space
lalt + shift + cmd - 0x2C : "${space-move}" last
lalt + shift + cmd - 0x2B : "${space-move}" prev
lalt + shift + cmd - 0x2F : "${space-move}" next
lalt + shift + cmd - 1    : "${space-move}" 1
lalt + shift + cmd - 2    : "${space-move}" 2
lalt + shift + cmd - 3    : "${space-move}" 3
lalt + shift + cmd - 4    : "${space-move}" 4
lalt + shift + cmd - 5    : "${space-move}" 5
lalt + shift + cmd - 6    : "${space-move}" 6
lalt + shift + cmd - 7    : "${space-move}" 7
lalt + shift + cmd - 8    : "${space-move}" 8
lalt + shift + cmd - 9    : "${space-move}" 9

# focus window within stack
lalt + ctrl - 0x2B : yabai -m window --focus stack.prev
lalt + ctrl - 0x2F : yabai -m window --focus stack.next

# move window within stack
lalt + shift + ctrl - 0x2B : yabai -m window --swap stack.prev && yabai -m window --focus stack.prev
lalt + shift + ctrl - 0x2F : yabai -m window --swap stack.next && yabai -m window --focus stack.next

# create spaces
lalt + cmd - 0x2C : "${space-create}" last
lalt + cmd - 0x2B : "${space-create}" prev
lalt + cmd - 0x2F : "${space-create}" next

# cycle windows
lalt         - tab : "${window-cycle}" reverse
lalt + shift - tab : "${window-cycle}" '.'

# close window
lalt - q : yabai -m window --close

# destroy space
lalt + cmd - q : "${space-destroy}"

# balance size of windows
lalt        - 0 : yabai -m space --balance
size < lalt - 0 : yabai -m space --balance

# rotate tree
lalt         - r : yabai -m space --rotate 90
lalt + shift - r : yabai -m space --rotate 270

# mirror tree y-axis
lalt - y : yabai -m space --mirror y-axis

# mirror tree x-axis
lalt - x : yabai -m space --mirror x-axis

# transpose split type
lalt - t : yabai -m window --toggle split

# toggle window parent zoom
lalt - m : yabai -m window --toggle zoom-parent; "${window-focus}"

# toggle window fullscreen zoom
lalt - f : yabai -m window --toggle zoom-fullscreen; "${window-focus}"

# toggle window native fullscreen
lalt + shift - f : yabai -m window --toggle native-fullscreen; "${window-focus}"

# float / unfloat window and center on screen
lalt + shift - space : \
    yabai -m window --toggle float &&   \
    yabai -m window --grid 4:4:1:1:2:2; \
    "${window-focus}"

# toggle sticky
lalt + shift - s : yabai -m window --toggle sticky

lalt - 0x2A : "${gaps-toggle}"

# toggle sticky, float and resize to picture-in-picture size
lalt - p : yabai -m window --toggle sticky && \
           yabai -m window --grid 5:5:4:0:1:1 \

# toggle mission control (application-level)
lalt - a : yabai -m window --toggle expose

.blacklist [
    "VMware Fusion"
]
''
