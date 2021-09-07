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
    then i3-msg "rename workspace to \"$trimmed_input\""
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

trim()
{
    suffix_trimmed="''${1%%*( )}"
    echo "''${suffix_trimmed##*( )}"
}


main
''
