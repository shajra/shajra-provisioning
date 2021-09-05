self: _super:

let
    progName = "shajra-nixos-rebuild";
    meta.description = "Controlled NixOS rebuild";
    sources = (import ../../../.. {}).sources;
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        coreutils
        hostname
        man_db
    ];
}
''
set -eu
set -o pipefail


TARGET="$(hostname)"
NIXOS_EXE="$(command -v nixos-rebuild || true)"
ARGS=()


. "${self.nix-project-lib.common}/share/nix-project/common.bash"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [OPTION]... [--] NIXOS_REBUILD_ARGS...

DESCRIPTION:

    A wrapper of nixos-rebuild that isolates Nixpkgs and NixOS
    configuration to pinned versions.  Unrecognized switches and
    arguments are passed through to nixos-rebuild.

OPTIONS:

    -h --help            print this help message
    -t --target          target host to configure for
                         (otherwise autodetected)
    -N --nixos-exe PATH  filepath of 'nixos-rebuild' executable to use

    '${progName}' pins all dependencies except for Nix itself,
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
        -t|--target)
            TARGET="''${2:-}"
            if [ -z "$TARGET" ]
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
    then rebuild "''${ARGS[@]}"
    else rebuild build
    fi
}

rebuild()
{
    /usr/bin/env -i \
        MANPATH=/run/current-system/sw/share/man \
        PATH="$(path_for "$NIXOS_EXE"):$PATH" \
        NIX_PATH="nixpkgs=${sources.nixpkgs-system}" \
        NIXOS_CONFIG="${sources.shajra-provisioning}/machines/$TARGET/configuration.nix" \
        nixos-rebuild "$@"
}


main "$@"
''
