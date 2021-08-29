{ self
, pname
, meta
, unasar-patched
}:

self.nix-project-lib.writeShellCheckedExe pname
{
    inherit meta;
    path = with self; [
        coreutils
        daemon
        electron_9
    ];
}
''
set -eu
set -o pipefail


. "${self.nix-project-lib.lib-sh}/share/nix-project/lib.sh"


COMMAND=start
DAEMON_NAME=blue_controller
ARGS=()


print_usage()
{
    cat - <<EOF
USAGE: ${pname} [OPTIONS]... [start | stop | toggle] [-- DAEMON_ARGS...]

DESCRIPTION:

    Runs the BluOS Controller, ensuring that there's only
    one instance running using a PID file.  This is done
    with the 'daemon' program.

    You can give a start or stop command.  Otherwise, start
    is assumed.  If you give both commands, the last one has
    precedence.

OPTIONS:

    -h --help            print this help message
    -H --help-daemon     print this help message

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
        -H|--help-daemon)
            daemon --help
            exit 0
            ;;
        --start|start)
            COMMAND=start
            ;;
        --stop|stop)
            COMMAND=stop
            ;;
        --toggle|toggle)
            COMMAND=toggle
            ;;
        --)
            shift
            ARGS+=("$@")
            break
            ;;
        *) die "unrecognized command: $1" ;;
        esac
        shift
    done
    case "$COMMAND" in
    start)  start_controller ;;
    stop)   stop_controller  ;;
    toggle) toggle_controller  ;;
    esac
}

start_controller()
{
    daemon "''${ARGS[@]}" --name "$DAEMON_NAME" -- electron "${unasar-patched}"
}

stop_controller()
{
    daemon "''${ARGS[@]}" --name "$DAEMON_NAME" --stop
}

toggle_controller()
{
    if daemon --name "$DAEMON_NAME" --running
    then stop_controller
    else start_controller
    fi
}


main "$@"
''
