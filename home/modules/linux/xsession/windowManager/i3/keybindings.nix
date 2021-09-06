mod: config: pkgs: kitty:

let
    autorandr = "${pkgs.autorandr}/bin/autorandr";
    awk = "${pkgs.gawk}/bin/awk";
    clipmenu = "${config.services.clipmenu.package}/bin/clipmenu";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    dunst-osd = "${pkgs.dunst-osd}/bin/dunst-osd";
    firefox = "${config.programs.firefox.package}/bin/firefox";
    fish = "${config.programs.fish.package}/bin/fish";
    i3-workspace-name = "${pkgs.i3-workspace-name}/bin/i3-workspace-name";
    pkill = "${pkgs.procps}/bin/pkill";
    rofi = "${config.programs.rofi.package}/bin/rofi";
    user = config.home.username;

    rofi-clip = pkgs.writers.writeDash "rofi-clip" ''
        exec "${rofi}" -dmenu -p 'clip' "$@"
    '';

    maim-select = pkgs.writers.writeDash "maim-select" ''
        TARGET=~/tmp/screenshots
        mkdir --parents "$TARGET"
        exec "${pkgs.maim}/bin/maim" --select \
            "$TARGET/screenshot.$(${pkgs.coreutils}/bin/date +%s).png"
    '';

in

{
    # split in horizontal orientation
    "${mod}+bracketright" = "split h";

    # split in vertical orientation
    "${mod}+bracketleft" = "split v";

    # toggle split
    "${mod}+backslash" = "layout toggle all";

    # fullscreen
    "${mod}+f" = "fullscreen";

    # global fullscreen
    "${mod}+Shift+f" = "fullscreen";

    # stacking
    "${mod}+s" = "layout stacking";

    # tabbed
    "${mod}+t" = "layout tabbed";

    # default
    "${mod}+d" = "layout default";

    # toggle tiling/floating of the current window
    "${mod}+Shift+space" = "floating toggle";

    # toggle focus between tiling and floating layers
    "${mod}+space" = "focus mode_toggle";

    # Make the currently focused window a scratchpad
    "${mod}+Shift+minus" = "move scratchpad";

    # Show the first scratchpad window
    "${mod}+minus" = "scratchpad show";

    # Stick toggle
    "${mod}+Shift+s" = "sticky toggle";

    # focus with Vim-style keys or cursor keys
    "${mod}+h" = "focus left";
    "${mod}+j" = "focus down";
    "${mod}+k" = "focus up";
    "${mod}+l" = "focus right";
    "${mod}+Left" = "focus left";
    "${mod}+Down" = "focus down";
    "${mod}+Up" = "focus up";
    "${mod}+Right" = "focus right";

    # move with Vim-style keys or cursor keys
    "${mod}+Shift+h" = "move left";
    "${mod}+Shift+j" = "move down";
    "${mod}+Shift+k" = "move up";
    "${mod}+Shift+l" = "move right";
    "${mod}+Shift+Left" = "move left";
    "${mod}+Shift+Down" = "move down";
    "${mod}+Shift+Up" = "move up";
    "${mod}+Shift+Right" = "move right";

    # move workspace with Vim-style keys or cursor keys
    "${mod}+Control+h" = "move workspace to output left";
    "${mod}+Control+j" = "move workspace to output down";
    "${mod}+Control+k" = "move workspace to output up";
    "${mod}+Control+l" = "move workspace to output right";
    "${mod}+Control+Left" = "move workspace to output left";
    "${mod}+Control+Down" = "move workspace to output down";
    "${mod}+Control+Up" = "move workspace to output up";
    "${mod}+Control+Right" = "move workspace to output right";

    # focus on the parent container
    "${mod}+a" = "focus parent";

    # focus on the child container
    "${mod}+Shift+a" = "focus child";

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

    # move to workspace
    "${mod}+Shift+1" = "move workspace number 1";
    "${mod}+Shift+2" = "move workspace number 2";
    "${mod}+Shift+3" = "move workspace number 3";
    "${mod}+Shift+4" = "move workspace number 4";
    "${mod}+Shift+5" = "move workspace number 5";
    "${mod}+Shift+6" = "move workspace number 6";
    "${mod}+Shift+7" = "move workspace number 7";
    "${mod}+Shift+8" = "move workspace number 8";
    "${mod}+Shift+9" = "move workspace number 9";
    "${mod}+Shift+0" = "move workspace number 10";

    # cycle through workspaces
    "${mod}+comma" = "workspace prev";
    "--whole-window ${mod}+button8" = "workspace prev";
    "${mod}+period" = "workspace next";
    "--whole-window ${mod}+button9" = "workspace next";
    "${mod}+slash" = "workspace back_and_forth";

    # rename workspace
    "${mod}+w" = ''exec ${i3-workspace-name}'';

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
    "${mod}+Control+n" = ''exec ${dunstctl} set-paused toggle'';

    # start a new terminal
    "${mod}+Return" = ''exec ${kitty} --single-instance'';

    # run dmenu run launcher
    "${mod}+F2" = ''exec ${rofi} -show window'';
    "${mod}+Tab" = ''exec ${rofi} -show window'';

    # run dmenu run launcher
    "${mod}+F3" = ''exec ${rofi} -show run'';
    "${mod}+p" = ''exec ${rofi} -show run'';

    # run dmenu ssh launcher
    "${mod}+F4" = ''exec ${rofi} -show ssh -terminal "${kitty} --single-instance"'';
    "${mod}+g" = ''exec ${rofi} -show ssh -terminal "${kitty} --single-instance"'';

    # clipboard management
    "${mod}+c" = ''exec CM_LAUNCHER=${rofi-clip} ${clipmenu}'';

    # kill the current client
    "${mod}+q" = "kill";

    # quit i3
    "${mod}+Control+Shift+q" = "exit";

    # reload i3 configuration
    "${mod}+Shift+r" = "reload";

    # reload i3 configuration
    "${mod}+Control+r" = ''exec ${autorandr} --change --default home'';

    # restart i3 inplace
    "${mod}+Control+Shift+r" = "restart";

    # resize mode
    "${mod}+r" = "mode \"resize\"";

    # passthrough mode
    "${mod}+Escape" = "mode \"passthrough\"";

    # marks
    "${mod}+Shift+m" = "move window to mark *; unmark *";
    "${mod}+m" = "mark --toggle *";

    # PulseAudio controls
    "XF86AudioRaiseVolume" = ''exec --no-startup-id ${dunst-osd} volume up'';
    "XF86AudioLowerVolume" = ''exec --no-startup-id ${dunst-osd} volume down'';
    "XF86AudioMute" =        ''exec --no-startup-id ${dunst-osd} volume mute-toggle'';

    # screen brightness
    "XF86MonBrightnessUp"   = ''exec --no-startup-id ${dunst-osd} brightness up'';
    "XF86MonBrightnessDown" = ''exec --no-startup-id ${dunst-osd} brightness down'';

    # media
    #"XF86AudioNext" = ''exec --no-startup-id ${mpc} next'';
    #"XF86AudioPlay" = ''exec --no-startup-id ${mpc} toggle'';
    #"XF86AudioPrev" = ''exec --no-startup-id ${mpc} prev'';
    "XF86Search" = ''exec ${firefox}'';

    # screen capture
    "${mod}+Control+s" = ''exec ${maim-select}'';
}
