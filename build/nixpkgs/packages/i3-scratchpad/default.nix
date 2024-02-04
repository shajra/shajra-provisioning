{ nix-project-lib
, coreutils
, i3
, jq
}:

let
    progName = "i3-scratchpad";
    meta.description = "Manage I3 scratchpad";
in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    path = [
        coreutils
        i3
        jq
    ];
}
''
set -eu
set -o pipefail


main()
{
    case "''${1:-}" in
        move) move ;;
        *) cycle ;;
    esac
}

move()
{
    if is_in_scratchpad
    then
        i3-msg floating toggle
    else
        i3-msg move scratchpad
        i3-msg scratchpad show
    fi
}

cycle()
{
    if is_in_scratchpad
    then i3-msg "scratchpad show; scratchpad show"
    else i3-msg focus next
    fi
}

is_in_scratchpad()
{
    i3-msg -t get_tree \
    | jq --exit-status '
        ..
        | select(.scratchpad_state? != "none")
        | .nodes[]?
        | select(.focused == true)
        | .id' >/dev/null
}


main "$@"
''
