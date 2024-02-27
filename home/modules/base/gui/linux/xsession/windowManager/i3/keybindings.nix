config: pkgs: mod: modAlt: kitty:

let

    autorandr = "${pkgs.autorandr}/bin/autorandr";
    awk = "${pkgs.gawk}/bin/awk";
    browser = "${pkgs.microsoft-edge-dev}/bin/microsoft-edge-dev";
    clipmenu = "${config.services.clipmenu.package}/bin/clipmenu";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    dunst-osd = "${pkgs.dunst-osd}/bin/dunst-osd";
    fish = "${config.programs.fish.package}/bin/fish";
    flameshot = "${config.services.flameshot.package}/bin/flameshot";
    i3-workspace-name = "${pkgs.i3-workspace-name}/bin/i3-workspace-name";
    i3-scratchpad = "${pkgs.i3-scratchpad}/bin/i3-scratchpad";
    pkill = "${pkgs.procps}/bin/pkill";
    rofi = "${config.programs.rofi.finalPackage}/bin/rofi";
    rofi-pass = "${config.programs.rofi.pass.package}/bin/rofi-pass";
    user = config.home.username;

    rofi-clip = pkgs.writers.writeDash "rofi-clip" ''
        exec "${rofi}" -dmenu -p 'clip' "$@"
    '';

in

{
    # resize mode
    "${mod}+s" = "mode \"resize\"";

    # passthrough mode
    "${mod}+Escape" = "mode \"passthrough\"";

    # focus with Vim-style keys or cursor keys
    "${mod}+h"     = "focus left";
    "${mod}+j"     = "focus down";
    "${mod}+k"     = "focus up";
    "${mod}+l"     = "focus right";
    "${mod}+Left"  = "focus left";
    "${mod}+Down"  = "focus down";
    "${mod}+Up"    = "focus up";
    "${mod}+Right" = "focus right";

    # move with Vim-style keys or cursor keys
    "${mod}+Shift+h"     = "move left";
    "${mod}+Shift+j"     = "move down";
    "${mod}+Shift+k"     = "move up";
    "${mod}+Shift+l"     = "move right";
    "${mod}+Shift+Left"  = "move left";
    "${mod}+Shift+Down"  = "move down";
    "${mod}+Shift+Up"    = "move up";
    "${mod}+Shift+Right" = "move right";

    # move workspace with Vim-style keys or cursor keys
    "${mod}+Control+h"     = "move workspace to output left";
    "${mod}+Control+j"     = "move workspace to output down";
    "${mod}+Control+k"     = "move workspace to output up";
    "${mod}+Control+l"     = "move workspace to output right";
    "${mod}+Control+Left"  = "move workspace to output left";
    "${mod}+Control+Down"  = "move workspace to output down";
    "${mod}+Control+Up"    = "move workspace to output up";
    "${mod}+Control+Right" = "move workspace to output right";

    # insertion orientation
    "${mod}+${modAlt}+j"  = "split v";
    "${mod}+${modAlt}+l"  = "split h";
    # REVISIT: Keep these?
    "${mod}+bracketleft"  = "split v";
    "${mod}+bracketright" = "split h";

    # focus workspace
    "${mod}+1" = "workspace number 1";
    "${mod}+2" = "workspace number 2";
    "${mod}+3" = "workspace number 3";
    "${mod}+4" = "workspace number 4";
    "${mod}+5" = "workspace number 5";
    "${mod}+6" = "workspace number 6";
    "${mod}+7" = "workspace number 7";
    "${mod}+8" = "workspace number 8";
    "${mod}+9" = "workspace number 9";
    "${mod}+0" = "workspace number 10";
    "${mod}+comma"                  = "workspace prev";
    "--whole-window ${mod}+button8" = "workspace prev";
    "${mod}+period"                 = "workspace next";
    "--whole-window ${mod}+button9" = "workspace next";
    "${mod}+slash"                  = "workspace back_and_forth";

    # move window to workspace
    "${mod}+Shift+1"      = "move workspace number 1";
    "${mod}+Shift+2"      = "move workspace number 2";
    "${mod}+Shift+3"      = "move workspace number 3";
    "${mod}+Shift+4"      = "move workspace number 4";
    "${mod}+Shift+5"      = "move workspace number 5";
    "${mod}+Shift+6"      = "move workspace number 6";
    "${mod}+Shift+7"      = "move workspace number 7";
    "${mod}+Shift+8"      = "move workspace number 8";
    "${mod}+Shift+9"      = "move workspace number 9";
    "${mod}+Shift+0"      = "move workspace number 10";
    "${mod}+Shift+comma"  = "move workspace prev";
    "${mod}+Shift+period" = "move workspace next";

    # move workspace
    "${mod}+${modAlt}+Shift+1"      = "exec ${i3-workspace-name} 1";
    "${mod}+${modAlt}+Shift+2"      = "exec ${i3-workspace-name} 2";
    "${mod}+${modAlt}+Shift+3"      = "exec ${i3-workspace-name} 3";
    "${mod}+${modAlt}+Shift+4"      = "exec ${i3-workspace-name} 4";
    "${mod}+${modAlt}+Shift+5"      = "exec ${i3-workspace-name} 5";
    "${mod}+${modAlt}+Shift+6"      = "exec ${i3-workspace-name} 6";
    "${mod}+${modAlt}+Shift+7"      = "exec ${i3-workspace-name} 7";
    "${mod}+${modAlt}+Shift+8"      = "exec ${i3-workspace-name} 8";
    "${mod}+${modAlt}+Shift+9"      = "exec ${i3-workspace-name} 9";
    "${mod}+${modAlt}+Shift+0"      = "exec ${i3-workspace-name} 10";
    "${mod}+${modAlt}+Shift+comma"  = "exec ${i3-workspace-name} prev";
    "${mod}+${modAlt}+Shift+period" = "exec ${i3-workspace-name} next";

    # focus on the parent container (up)
    "${mod}+u" = "focus parent";

    # focus on the child container (shift+up ≈ down)
    "${mod}+Shift+u" = "focus child";

    # open general launcher
    "${mod}+space" = "exec \"${rofi}"
        + " -show combi"
        + " -modes combi"
        + " -combi-modes \\\\\"drun,run,window,ssh,emoji\\\\\""
        + " -terminal \\\\\"${kitty} --single-instance\\\\\"\"";

    # open terminal
    "${mod}+Return" = ''exec ${kitty} --single-instance'';

    # kill the current client
    "${mod}+q" = "kill";

    # kill frame with mouse click on titlebar
    "button2" = ''--release kill'';

   # toggle tabbed
    "${mod}+t" = "layout toggle tabbed split";

    # cycle through all layouts
    "${mod}+Shift+t" = "layout toggle all";

    # rotate split
    "${mod}+r" = "layout toggle split";

    # fullscreen
    "${mod}+f" = "fullscreen";

    # toggle focus between tiling and floating layers
    "${mod}+minus" = "focus mode_toggle";

    # toggle tiling/floating of the current window
    "${mod}+Shift+minus" = "floating toggle";

    # Show scratchpad
    "${mod}+equal" = "scratchpad show";

    # Make the currently focused window a scratchpad
    "${mod}+Shift+equal" = "exec ${i3-scratchpad} move";

    # Cycle through scratchpad (or all windows)
    "${mod}+Tab" = "exec ${i3-scratchpad} cycle";

    # toggle sticky
    "${mod}+Shift+s" = "sticky toggle";

    # rename workspace
    "${mod}+w" = "exec ${i3-workspace-name}";

    # marks
    "${mod}+m" = "mark --toggle *";
    "${mod}+Shift+m" = "move window to mark *; unmark *";

    # notification center
    "${mod}+n" = ''
        exec test "$(${dunstctl} is-paused)" = false \
        && ${dunstctl} close \
        && ${pkill} -u ${user} -SIGRTMIN+0 i3status-rs
    '';
    "${mod}+Shift+n" = ''
        exec test "$(${dunstctl} is-paused)" = false \
        && ${dunstctl} history-pop \
        && ${pkill} -u ${user} -SIGRTMIN+0 i3status-rs
    '';
    "${mod}+${modAlt}+n" = ''exec ${dunstctl} action'';
    "${mod}+${modAlt}+Shift+n" = ''exec ${dunstctl} context'';
    "${mod}+Control+n" = ''exec ${dunstctl} set-paused toggle'';

    # clipboard management
    "${mod}+c" = ''exec CM_LAUNCHER=${rofi-clip} ${clipmenu}'';

    # password management
    "${mod}+p" = ''exec ${rofi-pass}'';

    # PulseAudio controls
    "XF86AudioRaiseVolume" = ''exec --no-startup-id ${dunst-osd} volume up'';
    "XF86AudioLowerVolume" = ''exec --no-startup-id ${dunst-osd} volume down'';
    "XF86AudioMute" =        ''exec --no-startup-id ${dunst-osd} volume mute-toggle'';

    # screen brightness
    "XF86MonBrightnessUp"   = ''exec --no-startup-id ${dunst-osd} brightness up'';
    "XF86MonBrightnessDown" = ''exec --no-startup-id ${dunst-osd} brightness down'';

    # media
    "XF86Search" = ''exec ${browser}'';

    # screen capture
    "${mod}+${modAlt}+s" = ''exec ${flameshot} gui'';

    # reload i3 configuration
    "${mod}+Shift+r" = "reload";

    # reload i3 configuration via autorand
    "${mod}+Control+r" = ''exec ${autorandr} --change --default home'';

    # restart i3 inplace
    "${mod}+Control+Shift+r" = "restart";

    # quit i3
    "${mod}+Control+Shift+q" = "exit";
}
