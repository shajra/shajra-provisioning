self: super:

let
    progName = "i3-dpi";
    meta.description = "set DPI for I3 window manager";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    # DESIGN: intentionally letting xorg.xrandr and xorg.xrdb come from
    # /run/current-system.  This guards against incompatibility of X between
    # nixpkgs-stable and nixpkgs-unstable.
    pathPure = false;
    path = with self; [
        coreutils
        i3
    ];
}
''
set -eu
set -o pipefail

PATH="$PATH:/run/current-system/sw/bin"

DPI="''${1:-235}"
XRESOURCES="$HOME/.Xresources.dpi"

echo "*dpi: $DPI" > "$XRESOURCES"
xrdb -merge "$XRESOURCES"
xrandr --dpi "$DPI"
i3-msg restart
''
