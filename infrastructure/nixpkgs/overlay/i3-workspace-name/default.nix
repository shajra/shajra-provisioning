self: super:

let
    progName = "i3-workspace-name";
    meta.description = "Change I3 workspace name interactively";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    path = with self; [
        coreutils
        i3
        jq
        rofi
    ];
}
''
set -eu
set -o pipefail


main()
{
    local input; input="$(get_input)"
    local trimmed_input; trimmed_input="$(trim "$input")"
    if [ -n "$trimmed_input" ]
    then smart_rename "$trimmed_input"
    fi
}

get_input()
{
    rofi -dmenu \
        -lines 0 \
        -p 'workspace name: ' \
        -theme-str '
            inputbar { children: [prompt, entry]; }
            listview { lines: 0; }
        '
}

smart_rename()
{
    local target_name="$1"
    local temp_name="$1_"
    local current_name; current_name="$(get_current_name)"
    if name_exists "$target_name"
    then
        rename "$current_name" "$temp_name"
        rename "$target_name" "$current_name"
        rename "$temp_name" "$target_name"
    else
        rename "$current_name" "$target_name"
    fi
}

get_current_name()
{
    i3-msg -t get_workspaces \
        | jq --raw-output '.[] | select(.focused == true).name'
}

name_exists()
{
    local name="$1"
    i3-msg -t get_workspaces \
        | jq --exit-status ".[] | select(.name == \"$name\")" \
        >/dev/null

}

rename()
{
    orig="$1"
    new="$2"
    i3-msg "rename workspace \"$orig\" to \"$new\""
}

trim()
{
    suffix_trimmed="''${1%%*( )}"
    echo "''${suffix_trimmed##*( )}"
}


main
''
