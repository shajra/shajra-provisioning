{ nix-project-lib
, bat
, chafa
, coreutils
, file
, gawk
, gnused
, kitty
}:

let

    progName = "fzf-preview-file";
    meta.description = "Previewer for FZF";

in

nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    path = [
        bat
        chafa
        coreutils
        file
        gawk
        gnused
        kitty
    ];
}
''
set -eu
set -o pipefail

. "${nix-project-lib.scriptCommon}/share/nix-project/common.sh"

FILE=

print_usage()
{
    cat - <<EOF
USAGE: ${progName} [-h|--help] FILE
EOF
}

main()
{
    parse_args "$@"
    validate_args
    preview_file
}

parse_args()
{
    while ! [ "''${1:-}" = "" ]
    do
        case "$1" in
        -h|--help)
            print_usage
            exit 0
            ;;
        *)
            FILE="$1"
            ;;
        esac
        shift
    done
}

validate_args()
{
    if [ -z "$FILE" ]
    then die "No file provided"
    fi
    if ! [ -r "$FILE" ]
    then die "Not a readable file: $FILE"
    fi
}

preview_file()
{
    local mime
    local type
    local encoding
    local dim
    mime="$(file --brief --dereference --mime-type -- "$FILE")"
    type="''${mime%/*}"
    encoding="$(file --brief --dereference --mime-encoding -- "$FILE")"
    if ! [ "$type" = image ]
    then
        if [ "$encoding" = binary ]
        then
            file "$FILE"
        else
            bat --color always    \
                --style numbers   \
                --pager never     \
                --wrap  never     \
                --line-range :300 \
                "$FILE"
        fi
    else
        dim="$(calculate_dimensions)"
        if [ -n "''${KITTY_WINDOW_ID:-}" ]
        then
            kitty icat                 \
                --clear                \
                --transfer-mode=memory \
                --unicode-placeholder  \
                --stdin=no             \
                --place="$dim@0x0"     \
                "$FILE"                \
            | sed '$d'
        else
            chafa -f sixel -s "$dim" "$FILE"
        fi
    fi
}

calculate_dimensions()
{
    local dim
    dim="''${FZF_PREVIEW_COLUMNS:-}x''${FZF_PREVIEW_LINES:-}"
    if [ "$dim" = x ]
    then
        dim="$(stty size < /dev/tty | awk '{print $2 "x" $1}')"
    elif [ -z "''${KITTY_WINDOW_ID:-}" ] \
        && (( FZF_PREVIEW_TOP + FZF_PREVIEW_LINES \
            == $(stty size < /dev/tty | awk '{print $1}') ))
    then
        dim="''${FZF_PREVIEW_COLUMNS:-}x$((FZF_PREVIEW_LINES - 1))"
    fi
    echo "$dim"
}

main "$@"
''
