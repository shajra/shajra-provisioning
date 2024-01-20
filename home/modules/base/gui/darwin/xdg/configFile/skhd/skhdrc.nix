config: pkgs: colors:

let

    jq    = "${pkgs.jq}/bin/jq";
    kitty = "${config.programs.kitty.package}/bin/kitty";

    refresh-apps-script = pkgs.writeShellScriptBin "refresh-apps" ''
        apps() {
            yabai -m query --windows --space "$1" \
            | "${jq}" -r '
                reduce .[].app as $app ({}; .[$app] += 1)
                | to_entries
                | map("[\"" + .key + "\"]=" + (.value|tostring))
                | "return{" + join(",") + "}"
            '
        }
        yabai -m query --spaces --display | "${jq}" '.[].index' | while read -r space
        do
            sketchybar --trigger space_windows_change \
                SPACE="$space" APPS="$(apps "$space")"
        done
    '';
    refresh-apps = "${refresh-apps-script}/bin/refresh-apps";

    space-move-script = pkgs.writeShellScriptBin "space-move" ''
        yabai -m space --move "$1" \
        && sketchybar --trigger space_change \
        && "${refresh-apps}"
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
        "${refresh-apps}"
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

in ''
# Strategy for keybindings:
#
# - left-only modifiers are used to allow right-variants when conflicts
# - lalt is used most often, lcmd as a small alternate
# - h/j/k/l is used for consistent directionality
# - digits are used for spaces and monitors
# - where possible shift implies moving of some sort
# - modifiers combinations are used in the following precedence:
#     - lalt
#     - lalt       + shift
#     - lcmd       + shift
#     - lalt + cmd
#     - lalt + cmd + shift
#
# NOTE: some keys are mysterious codes.  Use "skhd -o" to discover what these
# codes should be.

# define modes
:: default    : \
    borders \
        width=8.0 \
        active_color=${colors.window.selected.focused.border.window} ; \
    sketchybar --bar border_color=${colors.semantic.unifying}
:: passthru   : sketchybar --bar border_color=${colors.semantic.warning}
:: size     @ : borders width=8.0 active_color=${colors.semantic.info}

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

# create spaces
lalt + cmd - 0x2C : "${space-create}" last
lalt + cmd - 0x2B : "${space-create}" prev
lalt + cmd - 0x2F : "${space-create}" next

# close window
lalt - q : yabai -m window --close

# destroy space
lalt + cmd - q : "${space-destroy}"

# balance size of windows
lalt - 0 : yabai -m space --balance
size < lalt - 0 : yabai -m space --balance

# rotate tree
lalt - r : yabai -m space --rotate 90

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

# toggle sticky, float and resize to picture-in-picture size
lalt - p : yabai -m window --toggle sticky && \
           yabai -m window --grid 5:5:4:0:1:1 \

# toggle mission control (application-level)
lalt - a : yabai -m window --toggle expose

.blacklist [
    "VMware Fusion"
]
''
