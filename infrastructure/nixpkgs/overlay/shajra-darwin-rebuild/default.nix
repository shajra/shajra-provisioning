self: _super:

let
    progName = "shajra-darwin-rebuild";
    meta.description = "Controlled MacOS rebuild";
    sources = (import ../../../.. {}).sources;
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        coreutils
        hostname
        gnutar
        gzip
    ];
}
''
set -eu
set -o pipefail


TARGET="$(hostname)"
NIX_EXE="$(command -v nix || true)"
ARGS=()


. "${self.nix-project-lib.common}/share/nix-project/common.bash"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} [OPTION]... [--] NIX_DARWIN_ARGS...

DESCRIPTION:

    A wrapper of nix-darwin that isolates Nixpkgs and NixOS
    configuration to pinned versions.  Unrecognized switches and
    arguments are passed through to nix-darwin.

OPTIONS:

    -h --help         print this help message
    -t --target NAME  target host to configure for
                      (otherwise autodetected)
    -N --nix PATH     filepath of 'nix' executable to use

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
            if [ -z "''${2:-}" ]
            then die "$1 requires argument"
            fi
            TARGET="''${2:-}"
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
    local config="${sources.shajra-provisioning}/machines/$TARGET/darwin-configuration.nix"

    add_nix_to_path "$NIX_EXE"

    NIX_PATH="darwin=${sources.nix-darwin}"
    NIX_PATH="nixpkgs=${sources.nixpkgs-system}:$NIX_PATH"
    NIX_PATH="darwin-config=$config:$NIX_PATH"

    /usr/bin/env -i \
        "PATH=$PATH" \
        "NIX_PATH=$NIX_PATH" \
        nix build \
        --file "${sources.nix-darwin}" \
        --arg nixpkgs "${sources.nixpkgs-system}" \
        --arg configuration "$config" \
        --no-link \
        system

    local nix_darwin
    nix_darwin="$( /usr/bin/env -i \
        "PATH=$PATH" \
        "NIX_PATH=$NIX_PATH" \
        nix path-info \
        --file "${sources.nix-darwin}" \
        --arg nixpkgs "${sources.nixpkgs-system}" \
        --arg configuration "$config" \
        system
    )"

    /usr/bin/env -i \
        "NIX_PATH=$NIX_PATH" \
        "$nix_darwin/sw/bin/darwin-rebuild" \
        "$@"
}


main "$@"
''
