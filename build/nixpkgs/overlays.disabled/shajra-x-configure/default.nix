self: super:

let
    progName = "shajra-x-configure";
    meta.description = "configure X for my hotplugged input devices";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    runtimeShell = "${self.dash}/bin/dash";
    path = with self; [
        coreutils
    ];
}
''
set -eu


# DESIGN: intentionally letting xset and xinput come from /run/current-system.
# This guards against incompatibility of X between nixpkgs-stable and
# nixpkgs-unstable.
PATH="$PATH:/run/current-system/sw/bin"
export XAUTHORITY="''${XAUTHORITY:-/var/lib/lightdm/.Xauthority}"


main()
{
    configure_touchpad
    configure_mouse
    configure_trackball
    set_x
}

configure_touchpad()
{
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Natural Scrolling Enabled" 1
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Disable While Typing Enabled" 1
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Horizontal Scroll Enabled" 1
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Click Method Enabled" 0 1
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Accel Speed" 0.4
    xinput set-prop "SynPS/2 Synaptics TouchPad" \
        "libinput Click Method Enabled" 0 1
}

configure_mouse()
{
    xinput set-button-map "Kingsis Peripherals Evoluent VerticalMouse D" \
        1 2 3 4 5 6 7 9 10 8 11 12 13 14
    xinput set-prop "Kingsis Peripherals Evoluent VerticalMouse D" \
        "libinput Natural Scrolling Enabled" 1
    xinput set-prop "Kingsis Peripherals Evoluent VerticalMouse D" \
        "libinput Scroll Method Enabled" 0 0 1
    xinput set-prop "Kingsis Peripherals Evoluent VerticalMouse D" \
        "libinput Button Scrolling Button" 10
}

configure_trackball()
{
    xinput set-button-map "Kensington Expert Mouse" \
        1 2 3 4 5 6 7 8 9 10 11 12 13 14
    xinput set-prop "Kensington Expert Mouse" \
        "libinput Natural Scrolling Enabled" 1
    xinput set-prop "Kensington Expert Mouse" \
        "libinput Scroll Method Enabled" 0 0 1
    xinput set-prop "Kensington Expert Mouse" \
        "libinput Button Scrolling Button" 8
    xinput set-prop "Kensington Expert Mouse" \
        "libinput Accel Speed" 0.25
}

set_x()
{
    xset b off  # audible bells are annoying
    #xset s off  # disable screen saver
}


main
''
