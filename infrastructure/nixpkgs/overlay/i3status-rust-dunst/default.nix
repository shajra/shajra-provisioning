self: super:

let
    progName = "i3status-rust-dunst";
    meta.description = "I3status-rust integration with Dunst";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    path = with self; [
        bc
        coreutils
        dunst
    ];
}
''
set -eu
set -o pipefail


. "${self.nix-project-lib.common}/share/nix-project/common.bash"


# DESIGN: intentionally letting dbus-send come in from /run/current-system.
# This guards against incompatibility of X between nixpkgs-stable and
# nixpkgs-unstable.
PATH="$PATH:/run/current-system/sw/bin"
COMMAND=status


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [on | off | pause | unpause | toggle | status]

DESCRIPTION:

    Pauses or unpauses Dunst, and returns a JSON output useful for
    an I3status-rust custom block.  On/off are just aliases for
    unpause/pause.

    If no argument is provided a status is returned with no change.
    If you give multiple commands, the last one wins.

EOF
}

main()
{
    while ! [ "''${1:-}" = "" ]
    do
        case "$1" in
        -h|--help)
            print_usage
            exit 0
            ;;
        on|unpause)
            COMMAND=unpause
            ;;
        off|pause)
            COMMAND=pause
            ;;
        toggle)
            COMMAND=toggle
            ;;
        status)
            COMMAND=status
            ;;
        *)
            die "unrecognized argument: $1"
            ;;
        esac
        shift
    done
    do_command
}

do_command()
{
    case "$COMMAND" in
    status)  status ;;
    unpause) dunstctl set-paused false  ; status ;;
    pause)   dunstctl set-paused true   ; status ;;
    toggle)  dunstctl set-paused toggle ; status ;;
    esac
}

status()
{
    local count
    count="$(dunstctl count | cut -d : -f 2 | head -2 | paste -sd+ | bc)"
    local state=Idle
    if [ "$count" -gt 0 ]
    then state=Info
    else count=
    fi
    cat - <<EOF
    { "state": "$state"
    , "text": "$count"
    }
EOF
}


main "$@"
''
