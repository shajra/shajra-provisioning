{ coreutils
, jq
, nix-project-lib
, sketchybar
}:

let
    progName = "sketchybar-window-focus";
    meta.description = "Signal SketchyBar for latest window state";
in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = [
        coreutils
        jq
        sketchybar
    ];
}
''
set -eu
set -o pipefail

export PATH="/opt/homebrew/bin:$PATH"  # For yabai

yabai -m query --windows --window \
| jq -r '
    if .["is-floating"]           then "float"
    elif .["has-fullscreen-zoom"] then "fullscreen_zoom"
    elif .["has-parent-zoom"]     then "parent_zoom"
    elif .["stack-index"] > 0     then "stack"
    else "default"
    end
    , .["stack-index"]' \
| {
    read -r STATE
    read -r STACK_INDEX
    STACK_LAST=0
    if [ "$STACK_INDEX" -gt 0 ]
    then STACK_LAST="$(
        yabai -m query --windows --window stack.last \
        | jq '.["stack-index"]')"
    fi
    sketchybar --trigger window_focus \
        STATE="$STATE" \
        STACK_INDEX="$STACK_INDEX" \
        STACK_LAST="$STACK_LAST"
}
''
