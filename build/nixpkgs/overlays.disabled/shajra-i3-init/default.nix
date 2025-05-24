final: prev:

let
    progName = "shajra-i3-init";
    meta.description = "(re)initiatialize I3 to my preferences";
in

final.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    runtimeShell = "${final.dash}/bin/dash";
    path = with final; [
        shajra-x-configure
        i3-dpi
    ];
}
''
set -eu


shajra-x-configure
i3-dpi "$@"
''
