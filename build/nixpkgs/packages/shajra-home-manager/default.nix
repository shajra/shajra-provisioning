{ coreutils
, findutils
, git
, gnugrep
, gnutar
, gzip
, home-manager-latest
, hostname
, nix-project-lib
, sources
, which
}:

let
    progName = "shajra-home-manager";
    meta.description = "Controlled home directory management with Nix";
in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = [
        coreutils
        findutils
        git
        gnugrep
        gnutar
        gzip
        hostname
        home-manager-latest
        which
    ];
}
''
set -eu
set -o pipefail


FLAKE="${sources.shajra-provisioning}#$(hostname)"
HELP_RESPONSE=true
NIX_EXE="$(command -v /run/current-system/sw/bin/nix \
    ||     command -v /nix/var/nix/profiles/default/bin/nix \
    ||     true)"
ARGS=()


. "${nix-project-lib.scriptCommon}/share/nix-project/common.sh"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [OPTION]... [--] HOME_MANAGER_ARGS...

DESCRIPTION:

    A wrapper of home-manager that defaults to a pinned
    configuration.  Unrecognized switches and arguments are
    passed through to home-manager.

OPTIONS:

    -h --help         print this help message
    -f --flake REF    target configuration
                      (default autodetected by hostname)
    -N --nix PATH     filepath of 'nix' executable to use
    --slim            exclude packages for testing

    '${progName}' pins all dependencies except for Nix itself,
     which it finds on the path if possible.  Otherwise set
     '--nix'.

     To see the help of home-manager:
         ${progName} -- --help

EOF
}


main()
{
    while ! [ "''${1:-}" = "" ]
    do
        case "$1" in
        -h|--help)
            if "$HELP_RESPONSE"
            then
                print_usage
                exit 0
            else
                ARGS+=("$1")
            fi
            ;;
        -f|--flake)
            if [ -z "''${2:-}" ]
            then die "$1 requires argument"
            fi
            FLAKE="''${2:-}"
            shift
            ;;
        -N|--nix)
            if [ -z "''${2:-}" ]
            then die "$1 requires argument"
            fi
            NIX_EXE="''${2:-}"
            shift
            ;;
        --)
            HELP_RESPONSE=false
            ;;
        *)
            ARGS+=("$1")
            ;;
        esac
        shift
    done
    add_nix_to_path "$NIX_EXE"
    if [ "''${#ARGS[@]}" -gt 0 ]
    then manage "''${ARGS[@]}"
    else manage build
    fi
}

manage()
{
    local dbus_default; dbus_default="/run/user/$(id -u)/bus"
    local dbus_found=""
    if [ -e "$dbus_default" ]
    then dbus_found="unix:path=$dbus_default"
    fi
    /usr/bin/env -i \
        DBUS_SESSION_BUS_ADDRESS="''${DBUS_SESSION_BUS_ADDRESS:-''${dbus_found}}" \
        HOME="$HOME" \
        PATH="$PATH" \
        TERM="$TERM" \
        TERMINFO="''${TERMINFO:-}" \
        USER="$USER" \
        home-manager --flake "$FLAKE" "$@"
}


main "$@"
''
