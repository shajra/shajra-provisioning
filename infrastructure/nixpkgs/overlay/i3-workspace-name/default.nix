self: super:

let
    progName = "i3-workspace-name";
    meta.description = "change i3 workspace name interatively";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    # DESIGN: intentionally letting xorg.xrandr and xorg.xrdb come from
    # /run/current-system.  This guards against incompatibility of X between
    # nixpkgs-stable and nixpkgs-unstable.
    pathPure = false;
    path = with self; [
        coreutils
        i3
        rofi
    ];
}
''
set -eu
set -o pipefail


PATH="$PATH:/run/current-system/sw/bin"


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
        -p 'New name' \
        -theme-str '
            entry { placeholder: ""; }
            inputbar { children: [prompt, textbox-prompt-colon, entry]; }
        '
}

trim()
{
    suffix_trimmed="''${1%%*( )}"
    echo "''${suffix_trimmed##*( )}"
}


main
''
