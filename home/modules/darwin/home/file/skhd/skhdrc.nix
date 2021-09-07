kitty: jq:

''
# Strategy for keybindings:
#
# - left-only modifiers are used to allow right-variants when conflicts
# - lalt is used most often, lcmd as a small alternate
# - h/j/k/l is used for directionality consitently
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
:: default    : yabai -m config active_window_border_color 0xffb58900; yabai -m config window_border_width  4
:: passthru   : yabai -m config active_window_border_color 0xffd33682; yabai -m config window_border_width  8
:: size     @ : yabai -m config active_window_border_color 0xff859900; yabai -m config window_border_width 12
:: ins      @ : yabai -m config active_window_border_color 0xff268bd2; yabai -m config window_border_width 12

# mode: returning home
size, ins, passthru < lalt + shift - escape ; default
size, ins, passthru < lalt         - escape ; default
size, ins           <                escape ; default
size, ins           <                return ; default

# mode: resizing, with toggle back
lalt        - s ; size
size < lalt - s ; default

# mode: insertion, with toggle back
lalt       - i ; ins
ins < lalt - i ; default

# mode: passthrough, with toggle back
lalt            - escape ; passthru
passthru < lalt - escape ; default

# open terminal
#lalt - return : alacritty # no args
lalt - return : ${kitty} --single-instance --wait-for-single-instance-window-close --directory ~

# focus window
lalt - h : yabai -m window --focus west
lalt - j : yabai -m window --focus south
lalt - k : yabai -m window --focus north
lalt - l : yabai -m window --focus east

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
ins < lalt - i : yabai -m window --insert cancel
ins < lalt - h : yabai -m window --insert west
ins < lalt - j : yabai -m window --insert south
ins < lalt - k : yabai -m window --insert north
ins < lalt - l : yabai -m window --insert east

# swap window
lalt + shift - h : yabai -m window --swap west
lalt + shift - j : yabai -m window --swap south
lalt + shift - k : yabai -m window --swap north
lalt + shift - l : yabai -m window --swap east

# warp window
lcmd + shift - h : yabai -m window --warp west
lcmd + shift - j : yabai -m window --warp south
lcmd + shift - k : yabai -m window --warp north
lcmd + shift - l : yabai -m window --warp east

# move window
lalt + cmd - h : yabai -m window --move rel:-20:0
lalt + cmd - j : yabai -m window --move rel:0:20
lalt + cmd - k : yabai -m window --move rel:0:-20
lalt + cmd - l : yabai -m window --move rel:20:0

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
lcmd + shift - 0x2C : yabai -m window --space last; yabai -m space --focus last
lcmd + shift - 0x2B : yabai -m window --space prev; yabai -m space --focus prev
lcmd + shift - 0x2F : yabai -m window --space next; yabai -m space --focus next
lcmd + shift - 1 : yabai -m window --space  1; yabai -m space --focus 1
lcmd + shift - 2 : yabai -m window --space  2; yabai -m space --focus 2
lcmd + shift - 3 : yabai -m window --space  3; yabai -m space --focus 3
lcmd + shift - 4 : yabai -m window --space  4; yabai -m space --focus 4
lcmd + shift - 5 : yabai -m window --space  5; yabai -m space --focus 5
lcmd + shift - 6 : yabai -m window --space  6; yabai -m space --focus 6
lcmd + shift - 7 : yabai -m window --space  7; yabai -m space --focus 7
lcmd + shift - 8 : yabai -m window --space  8; yabai -m space --focus 8
lcmd + shift - 9 : yabai -m window --space  9; yabai -m space --focus 9

# focus monitor
lalt + cmd - 0x2C : yabai -m display --focus last
lalt + cmd - 0x2B : yabai -m display --focus prev
lalt + cmd - 0x2F : yabai -m display --focus next
lalt + cmd - 1 : yabai -m display --focus 1
lalt + cmd - 2 : yabai -m display --focus 2
lalt + cmd - 3 : yabai -m display --focus 3

# send window to monitor and follow focus
lalt + cmd + shift - 0x2C : yabai -m window --display last; yabai -m display --focus last
lalt + cmd + shift - 0x2B : yabai -m window --display prev; yabai -m display --focus prev
lalt + cmd + shift - 0x2F : yabai -m window --display next; yabai -m display --focus next
lalt + cmd + shift - 1 : yabai -m window --display 1; yabai -m display --focus 1
lalt + cmd + shift - 2 : yabai -m window --display 2; yabai -m display --focus 2
lalt + cmd + shift - 3 : yabai -m window --display 3; yabai -m display --focus 3


# rotate tree
lalt - r : yabai -m space --rotate 90

# mirror tree y-axis
lalt - y : yabai -m space --mirror y-axis

# mirror tree x-axis
lalt - x : yabai -m space --mirror x-axis

# toggle desktop offset
lalt - o : yabai -m space --toggle padding; yabai -m space --toggle gap

# close window
lalt - q : yabai -m window --close

# toggle window parent zoom
lalt - d : yabai -m window --toggle zoom-parent

# toggle window fullscreen zoom
lalt - f : yabai -m window --toggle zoom-fullscreen

# toggle window native fullscreen
lalt + shift - space : yabai -m window --toggle native-fullscreen

# toggle window border
lalt - b : yabai -m window --toggle border

# toggle window split type
lalt - e : yabai -m window --toggle split

# float / unfloat window and center on screen
lalt - t : yabai -m window --toggle float;\
           yabai -m window --grid 4:4:1:1:2:2

# toggle sticky
lalt + shift - s : yabai -m window --toggle sticky

# toggle sticky, float and resize to picture-in-picture size
lalt - p : yabai -m window --toggle sticky;\
          yabai -m window --grid 5:5:4:0:1:1

# toggle mission control (application-level)
lalt - a : yabai -m window --toggle expose

# layouts
lalt + shift - f : yabai -m space --layout float
lcmd + shift - f : yabai -m space --layout bsp

.blacklist [
    "VMware Fusion"
]
''
