{ coreutils
, git
, hostname
, less
, man-db
, nix-project-lib
, sources
}:

let
    progName = "shajra-nixos-rebuild";
    meta.description = "Controlled NixOS rebuild";
in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = [
        coreutils
        git
        hostname
        less
        man-db
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
NIXOS_EXE="$(command -v /run/current-system/sw/bin/nixos-rebuild \
    ||       true)"
ARGS=()


. "${nix-project-lib.scriptCommon}/share/nix-project/common.sh"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [OPTION]... [--] NIXOS_REBUILD_ARGS...

DESCRIPTION:

    A wrapper of nixos-rebuild that defaults to a pinned
    configuration.  Unrecognized switches and arguments are
    passed through to home-manager.

OPTIONS:

    -h --help                print this help message
    -f --flake REF           target configuration
                             (default autodetected by hostname)
    -N --nix PATH            filepath of 'nix' executable to use
    -R --nixos-rebuild PATH  filepath of 'nixos-rebuild'
                             executable to use

    '${progName}' pins all dependencies except for Nix itself,
     which it finds on the path if possible.  Otherwise set
     '--nix' and '--nixos-rebuild'.

     To see the help of nixos-rebuild:
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
        -R|--nixos-rebuild)
            if [ -z "''${2:-}" ]
            then die "$1 requires argument"
            fi
            NIXOS_EXE="''${2:-}"
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
    if [ "''${#ARGS[@]}" -gt 0 ]
    then rebuild "''${ARGS[@]}"
    else rebuild build
    fi
}

rebuild()
{
    /usr/bin/env -i \
        MANPATH=/run/current-system/sw/share/man \
        PATH="$(path_for "$NIX_EXE"):$(path_for "$NIXOS_EXE"):$PATH" \
        TERM="$TERM" \
        TERMINFO="''${TERMINFO:-}" \
        nixos-rebuild --flake "$FLAKE" "$@"
}


main "$@"
''
