{
  coreutils,
  git,
  hostname,
  less,
  man-db,
  nix-project-lib,
  sources,
}:

let
  progName = "shajra-nixos-rebuild";
  meta.description = "Controlled NixOS rebuild";
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    envKeep = [
      "LOCALE_ARCHIVE"
      "PATH"
      "TERM"
      "TERMINFO"
    ];
    pathKeep = [
      "nix"
      "nixos-rebuild"
      "systemd-run"
    ];
    pathPackages = [
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
    ARGS=()


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
        MANPATH=/run/current-system/sw/share/man \
            nixos-rebuild --flake "$FLAKE" "$@"
    }


    main "$@"
  ''
