self: super:

let
    progName = "shajra-i3-init";
    meta.description = "(re)initiatialize I3 to my preferences";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    runtimeShell = "${self.dash}/bin/dash";
    path = with self; [
        shajra-x-configure
        i3-dpi
    ];
}
''
set -eu


shajra-x-configure
i3-dpi "$@"
''
