{ nix-project-lib
, coreutils
, dash
, i3
}:

let
    progName = "i3-dpi";
    meta.description = "Change DPI for I3 window manager";
in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    runtimeShell = "${dash}/bin/dash";
    path = [
        coreutils
        i3
    ];
}
''
set -eu


# DESIGN: intentionally letting xorg.xrandr and xorg.xrdb come from
# /run/current-system.  This guards against incompatibility of X between
# nixpkgs-stable and nixpkgs-unstable.
PATH="$PATH:/run/current-system/sw/bin"

DPI="''${1:-235}"
XRESOURCES=~/.Xresources.dpi


main()
{
    configure_dpi
    restart_i3
}

configure_dpi()
{
    echo "*dpi: $DPI" > "$XRESOURCES"
    xrdb -merge "$XRESOURCES"
    xrandr --dpi "$DPI"
}

restart_i3()
{
    i3-msg restart
}


main
''
