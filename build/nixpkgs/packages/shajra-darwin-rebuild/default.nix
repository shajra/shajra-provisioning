{
  coreutils,
  hostname,
  gnutar,
  gzip,
  nix-project-lib,
  sources,
}:

let
  progName = "shajra-darwin-rebuild";
  meta.description = "Controlled MacOS rebuild";
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    envKeep = [
      "HOME"
      "PATH"
      "TERM"
      "TERMINFO"
      "SSH_AUTH_SOCK"
    ];
    pathKeep = [
      "nix"
    ];
    pathPackages = [
      coreutils
      hostname
      gnutar
      gzip
    ];
  }
  ''
    set -eu
    set -o pipefail


    BOOTSTRAP="${sources.shajra-provisioning}#darwinConfigurations.lemon-slim.system"
    FLAKE="${sources.shajra-provisioning}#$(hostname)"
    HELP_RESPONSE=true
    ARGS=()


    print_usage()
    {
        cat - <<EOF
    USAGE: ${progName} [OPTION]... [--] NIX_DARWIN_ARGS...

    DESCRIPTION:

        A wrapper of darwin-rebuild that defaults to a pinned
        configuration.  Unrecognized switches and arguments are
        passed through to home-manager.

    OPTIONS:

        -h --help         print this help message
        -f --flake REF    target configuration
                          (default autodetected by hostname)

         To see the help of darwin-rebuild:
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
        nix build --no-link "$BOOTSTRAP"
        local result; result="$(nix path-info "$BOOTSTRAP")"
        "$result/sw/bin/darwin-rebuild" --flake "$FLAKE" "$@"
    }


    main "$@"
  ''
