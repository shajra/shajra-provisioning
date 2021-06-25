self: super:

let
    prog_name = "shajra-nixos-rebuild";
    meta.description = "controlled NixOS rebuild";
    src = super.lib.sources.sourceFilesBySuffices ../../../..
        [ ".nix" ".json" ];
in

self.nix-project-lib.writeShellCheckedExe prog_name
{
    inherit meta;
}
''
set -eu
set -o pipefail


PROG="$("${self.coreutils}/bin/basename" "$0")"
HOST="$("${self.nettools}/bin/hostname")"
NIXOS_EXE="$(command -v nixos-rebuild || true)"
ARGS=()


. "${self.nix-project-lib.lib-sh}/share/nix-project/lib.sh"


print_usage()
{
    "${self.coreutils}/bin/cat" - <<EOF
USAGE:

    $PROG [OPTION]... NIXOS_REBUILD_ARGS...

DESCRIPTION:

    A wrapper of nixos-rebuild that isolates Nixpkgs and NixOS
    configuration to pinned versions.  Unrecognized switches and
    arguments are passed through to nixos-rebuild.

OPTIONS:

    -h --help            print this help message
    -H --host            name of host to configure for
                         (otherwise autodetected)
    -N --nixos-exe PATH  filepath of 'nixos-rebuild' executable to use

    '$PROG' pins all dependencies except for Nix itself,
     which it finds on the path if possible.  Otherwise set
     '--nixos-exe'.

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
        -H|--host)
            HOST="''${2:-}"
            if [ -z "$HOST" ]
            then die "$1 requires argument"
            fi
            shift
            ;;
        -N|--nixos-exe)
            NIXOS_EXE="''${2:-}"
            if [ -z "$NIXOS_EXE" ]
            then die "$1 requires argument"
            fi
            shift
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
        PATH="$(path_for "$NIXOS_EXE")" \
        NIX_PATH="nixpkgs=${super.path}" \
        NIXOS_CONFIG="${src}/machines/$HOST/configuration.nix" \
        nixos-rebuild "$@"
}


main "$@"
''
