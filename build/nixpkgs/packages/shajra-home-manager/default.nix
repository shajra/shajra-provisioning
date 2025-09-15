{
  coreutils,
  findutils,
  git,
  gnugrep,
  gnutar,
  gzip,
  home-manager-latest,
  hostname,
  nix-project-lib,
  sources,
  which,
}:

let
  progName = "shajra-home-manager";
  meta.description = "Controlled home directory management with Nix";
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    envKeep = [
      "DBUS_SESSION_BUS_ADDRESS"
      "HOME"
      "PATH"
      "TERM"
      "TERMINFO"
      "USER"
    ];
    pathKeep = [
      "nix"
    ];
    pathPackages = [
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
    ARGS=()


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
        --slim            exclude packages for testing

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
        DBUS_SESSION_BUS_ADDRESS="''${DBUS_SESSION_BUS_ADDRESS:-''${dbus_found}}" \
            home-manager --flake "$FLAKE" "$@"
    }


    main "$@"
  ''
