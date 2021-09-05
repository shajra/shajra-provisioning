self: _super:

let
    progName = "shajra-home-manager";
    meta.description = "Controlled home directory management with Nix";
    sources = (import ../../../.. {}).sources;
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        coreutils
        git
        gnugrep
        gnutar
        gzip
        hostname
        home-manager
    ];
}
''
set -eu
set -o pipefail


TARGET="$(hostname)"
NIX_EXE="$(command -v nixos-rebuild || true)"
ARGS=()


. "${self.nix-project-lib.common}/share/nix-project/common.bash"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [OPTION]... [--] NIXOS_REBUILD_ARGS...

DESCRIPTION:

    A wrapper of home-manager that heavily controls envioronment
    variables, including NIX_PATH.  Unrecognized switches and
    arguments are passed through to home-manager.

OPTIONS:

    -h --help         print this help message
    -t --target NAME  target configuration
                      (default autodetected by hostname)
    -N --nix PATH     filepath of 'nixos-rebuild' executable to use

    '${progName}' pins all dependencies except for Nix itself,
     which it finds on the path if possible.  Otherwise set
     '--nix'.

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
        -t|--target)
            TARGET="''${2:-}"
            if [ -z "$TARGET" ]
            then die "$1 requires argument"
            fi
            shift
            ;;
        -N|--nix)
            NIX_EXE="''${2:-}"
            if [ -z "$NIX_EXE" ]
            then die "$1 requires argument"
            fi
            shift
            ;;
        --)
            shift
            ARGS+=("$@")
            break
            ;;
        *)
            ARGS+=("$1")
            ;;
        esac
        shift
    done
    if [ "''${#ARGS[@]}" -gt 0 ]
    then manage "''${ARGS[@]}"
    else manage build
    fi
}

manage()
{
    local config="${sources.shajra-provisioning}/home/target/$TARGET"
    PATH="$(path_for "$NIX_EXE"):$PATH"
    /usr/bin/env -i \
        HOME="$HOME" \
        PATH="$PATH" \
        TERM="$TERM" \
        DBUS_SESSION_BUS_ADDRESS="''${DBUS_SESSION_BUS_ADDRESS:-}" \
        TERMINFO="''${TERMINFO:-}" \
        USER="$USER" \
        NIX_PATH="nixpkgs=${sources.nixpkgs-home}" \
        home-manager -f "$config" "$@"
}


main "$@"
''
