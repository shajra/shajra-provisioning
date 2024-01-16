config: pkgs: colors:

let

    jq    = "${pkgs.jq}/bin/jq";
    kitty = "${config.programs.kitty.package}/bin/kitty";

    swap-script = pkgs.writeShellScriptBin "swap" ''
        set -e
        set -o pipefail

        index() {
            yabai -m query --spaces "''${@}" | ${jq} .index
        }

        apps() {
            yabai -m query --windows "''${@}" \
            | ${jq} -r '
                reduce .[].app as $app ({}; .[$app] += 1)
                | to_entries
                | map("[\"" + .key + "\"]=" + (.value|tostring))
                | "return{" + join(",") + "}"
            '
        }

        OLD="$(index --space)"
        index --space "$1" > /dev/null
        NEW="$(index --space "$1")"
        yabai -m space --swap "$1"
        sketchybar --trigger space_change
        sketchybar --trigger space_windows_change \
            SPACE="$OLD" APPS="$(apps --space "$OLD")"
        sketchybar --trigger space_windows_change \
            SPACE="$NEW" APPS="$(apps --space "$NEW")"
    '';

    swap = "${swap-script}/bin/swap";

in ''
# Strategy for keybindings:
#
# - left-only modifiers are used to allow right-variants when conflicts
# - lalt is used most often, lcmd as a small alternate
# - h/j/k/l is used for consistent directionality
# - digits are used for desktops and monitors
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
lalt - h : yabai -m window  --focus west  \
        || yabai -m display --focus west
lalt - j : yabai -m window  --focus south \
        || yabai -m display --focus south
lalt - k : yabai -m window  --focus north \
        || yabai -m display --focus north
lalt - l : yabai -m window  --focus east  \
        || yabai -m display --focus east

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

# set insertion point in focused container
lalt + cmd - i : yabai -m window --insert cancel
lalt + cmd - h : yabai -m window --insert west
lalt + cmd - j : yabai -m window --insert south
lalt + cmd - k : yabai -m window --insert north
lalt + cmd - l : yabai -m window --insert east

# swap or move window
lalt + shift - h : yabai -m window --swap west  \
                || yabai -m window --move rel:-20:0
lalt + shift - j : yabai -m window --swap south \
                || yabai -m window --move rel:0:20
lalt + shift - k : yabai -m window --swap north \
                || yabai -m window --move rel:0:-20
lalt + shift - l : yabai -m window --swap east  \
                || yabai -m window --move rel:20:0

# warp window
lcmd + shift - h [
    "google chrome beta" ~
    * : yabai -m window --warp west
]
lcmd + shift - j [
    "google chrome beta" ~
    * : yabai -m window --warp south
]
lcmd + shift - k [
    "google chrome beta" ~
    * : yabai -m window --warp north
]
lcmd + shift - l [
    "google chrome beta" ~
    * : yabai -m window --warp east
]

# make floating window fill left-half of screen
lalt + cmd + shift - h : yabai -m window --grid 1:2:0:0:1:1
# make floating window fill screen
lalt + cmd + shift - k : yabai -m window --grid 1:1:0:0:1:1
# make floating window fill right-half of screen
lalt + cmd + shift - l : yabai -m window --grid 1:2:1:0:1:1

# balance size of windows
lalt - 0 : yabai -m space --balance
size < lalt - 0 : yabai -m space --balance

# create desktop, move window and follow focus
lalt + shift - n : yabai -m space --create;\
           id="$(yabai -m query --spaces --display \
               | "${jq}" 'map(select(."native-fullscreen" == 0))[-1].index')";\
           yabai -m window --space $id;\
           yabai -m space --focus $id

# create desktop and follow focus
lalt - n : yabai -m space --create;\
           id="$(yabai -m query --spaces --display \
               | "${jq}" 'map(select(."native-fullscreen" == 0))[-1].index')";\
           yabai -m space --focus $id

# destroy desktop
lalt + shift - w : yabai -m space --destroy

# fast focus desktop
lalt - 0x2C : yabai -m space --focus last
lalt - 0x2B : yabai -m space --focus prev
lalt - 0x2F : yabai -m space --focus next
lalt - 1 : yabai -m space --focus 1
lalt - 2 : yabai -m space --focus 2
lalt - 3 : yabai -m space --focus 3
lalt - 4 : yabai -m space --focus 4
lalt - 5 : yabai -m space --focus 5
lalt - 6 : yabai -m space --focus 6
lalt - 7 : yabai -m space --focus 7
lalt - 8 : yabai -m space --focus 8
lalt - 9 : yabai -m space --focus 9

# send window to desktop
lalt + shift - 0x2C : yabai -m window --space last
lalt + shift - 0x2B : yabai -m window --space prev
lalt + shift - 0x2F : yabai -m window --space next
lalt + shift - 1 : yabai -m window --space 1
lalt + shift - 2 : yabai -m window --space 2
lalt + shift - 3 : yabai -m window --space 3
lalt + shift - 4 : yabai -m window --space 4
lalt + shift - 5 : yabai -m window --space 5
lalt + shift - 6 : yabai -m window --space 6
lalt + shift - 7 : yabai -m window --space 7
lalt + shift - 8 : yabai -m window --space 8
lalt + shift - 9 : yabai -m window --space 9

# send window to desktop and follow focus
lcmd + shift - 0x2C [
    "google chrome beta" ~
    * : yabai -m window --space last \
     && yabai -m space --focus last
]
lcmd + shift - 0x2B [
    "google chrome beta" ~
    * : yabai -m window --space prev \
     && yabai -m space --focus prev
]
lcmd + shift - 0x2F [
    "google chrome beta" ~
    * : yabai -m window --space next \
     && yabai -m space --focus next
]
lcmd + shift - 1 [
    "google chrome beta" ~
    * : yabai -m window --space  1 \
     && yabai -m space --focus 1
]
lcmd + shift - 2 [
    "google chrome beta" ~
    * : yabai -m window --space  2 \
     && yabai -m space --focus 2
]
lcmd + shift - 3 [
    "google chrome beta" ~
    * : yabai -m window --space  3 \
     && yabai -m space --focus 3
]
lcmd + shift - 4 [
    "google chrome beta" ~
    * : yabai -m window --space  4 \
     && yabai -m space --focus 4
]
lcmd + shift - 5 [
    "google chrome beta" ~
    * : yabai -m window --space  5 \
     && yabai -m space --focus 5
]
lcmd + shift - 6 [
    "google chrome beta" ~
    * : yabai -m window --space  6 \
     && yabai -m space --focus 6
]
lcmd + shift - 7 [
    "google chrome beta" ~
    * : yabai -m window --space  7 \
     && yabai -m space --focus 7
]
lcmd + shift - 8 [
    "google chrome beta" ~
    * : yabai -m window --space  8 \
     && yabai -m space --focus 8
]
lcmd + shift - 9 [
    "google chrome beta" ~
    * : yabai -m window --space  9 \
     && yabai -m space --focus 9
]

# swap spaces
lalt + cmd - 0x2C : ${swap} last
lalt + cmd - 0x2B : ${swap} prev
lalt + cmd - 0x2F : ${swap} next
lalt + cmd - 1 : ${swap} 1
lalt + cmd - 2 : ${swap} 2
lalt + cmd - 3 : ${swap} 3
lalt + cmd - 4 : ${swap} 4
lalt + cmd - 5 : ${swap} 5
lalt + cmd - 6 : ${swap} 6
lalt + cmd - 7 : ${swap} 7
lalt + cmd - 8 : ${swap} 8
lalt + cmd - 9 : ${swap} 9

# rotate tree
lalt - r : yabai -m space --rotate 90

# mirror tree y-axis
lalt - y : yabai -m space --mirror y-axis

# mirror tree x-axis
lalt - x : yabai -m space --mirror x-axis

# toggle desktop offset
lalt - o : yabai -m space --toggle padding && yabai -m space --toggle gap

# close window
lalt - q : yabai -m window --close

# toggle window parent zoom
lalt - m : yabai -m window --toggle zoom-parent; \
           sketchybar --trigger window_focus

# toggle window fullscreen zoom
lalt - f : yabai -m window --toggle zoom-fullscreen; \
           sketchybar --trigger window_focus

# toggle window native fullscreen
lalt + shift - f : \
    yabai -m window --toggle native-fullscreen; \
    sketchybar --trigger window_focus

# toggle window split type
lalt - e : yabai -m window --toggle split

# float / unfloat window and center on screen
lalt + shift - space : \
    yabai -m window --toggle float &&   \
    yabai -m window --grid 4:4:1:1:2:2; \
    sketchybar --trigger window_focus

# toggle sticky
lalt + shift - s : yabai -m window --toggle sticky

# toggle sticky, float and resize to picture-in-picture size
lalt - p : yabai -m window --toggle sticky && \
           yabai -m window --grid 5:5:4:0:1:1 \

# toggle mission control (application-level)
lalt - a : yabai -m window --toggle expose

# DESIGN: not sure this is a good idea
# layout
#lalt + shift - f : yabai -m space --layout float
#lcmd + shift - f : yabai -m space --layout bsp

.blacklist [
    "VMware Fusion"
]
''
