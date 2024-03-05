{ nix-project-lib
, atool
, bat
, chafa
, coreutils
, eza
, ffmpeg
, ffmpegthumbnailer
, file
, gawk
, gnused
, imagemagick
, p7zip
, poppler_utils
, unrar
, unzip
, xxHash
}:

let

    progName = "preview-file";
    meta.description = "Terminal previewer";

in

# DESIGN: This script draws heavily from the following:
# https://gitlab.com/sourplum/dotfiles.
# https://github.com/junegunn/fzf/blob/master/bin/fzf-preview.sh
nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;

    path = [
        atool
        bat
        chafa
        coreutils
        eza
        ffmpeg
        ffmpegthumbnailer
        file
        gawk
        gnused
        imagemagick
        p7zip
        poppler_utils
        unrar
        unzip
        xxHash
    ];

}
''
set -eu
set -o pipefail

. "${nix-project-lib.scriptCommon}/share/nix-project/common.sh"

CACHE_DIR="$HOME/.cache/${progName}"
RATIO=768

FILE=
MIME_TYPE=
MIME_ENCODING=
EXTENSION=

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
    prep_cache_dir
    preview_clear
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
            MIME_TYPE=$(file --brief --dereference --mime-type -- "$FILE")
            MIME_ENCODING=$(file --brief  --dereference --mime-encoding -- "$FILE")
            EXTENSION="$(printf "%s" "''${1##*.}" | tr '[:upper:]' '[:lower:]')"
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

prep_cache_dir()
{
    mkdir --parents "$CACHE_DIR"
}

preview_clear()
{
    case "$(detect_terminal)" in
    kitty|kitty-*)
        # DESIGN: https://github.com/junegunn/fzf/issues/3228#issuecomment-1803402184
        printf "\x1b_Ga=d,d=A\x1b\\"
        ;;
    iterm|iterm-*)
        ;;
    *)
        echo
        # DESIGN: This magic string is a general textual clear for terminals.
        printf "\e[H\e[2J"
        ;;
    esac
}

preview_file()
{
    case "$MIME_TYPE" in
    image/*)         draw "$FILE"              ;;
    application/pdf) draw "$(pdf_thumbnail)"   ;;
    audio/*)         draw "$(audio_thumbnail)" ;;
    video/*)         draw "$(video_thumbnail)" ;;
    inode/directory) preview_dir               ;;
    *)
        case "$EXTENSION" in
        a|ace|alz|arc|arj|bz|bz2)    preview_archive           ;;
        cab|cpio|deb|gz|jar)         preview_archive           ;;
        lha|lz|lzh|lzma|lzo)         preview_archive           ;;
        rpm|rz|t7z|tar)              preview_archive           ;;
        tbz|tbz2|tgz|tlz|txz|tZ|tzo) preview_archive           ;;
        war|xpi|xz|Z|zip)            preview_archive           ;;
        rar)                         unrar lt -p- -- "$FILE"   ;;
        7z)                          7z l -p -- "$FILE"        ;;
        cbz)                         draw "$(comic_thumbnail)" ;;
        *)
            case "$MIME_ENCODING" in
            binary) file "$FILE" ;;
            *)      preview_text ;;
            esac
        esac
    esac
}

draw()
{
    if [ -f "$1" ]
    then
        case "$(detect_terminal)" in
        *-sixels)      chafa --format sixels --view-size "$(dimensions)" "$1" ;;
        kitty|kitty-*) chafa --format kitty  --view-size "$(dimensions)" "$1" ;;
        iterm|iterm-*) chafa --format iterm  --view-size "$(dimensions)" "$1" ;;
        *)
            # DESIGN: Extra clear and sleep is because of a race condition on
            # long loads.  This is only a problem for ASCII/ANSI rendering.
            # Slowing rendering intentionally is less than ideal, but 250ms is
            # tolerable.  Also, will be using Kitty or iTerm2 most of the time
            # anyway.
            timeout 0.25s sleep 1 || true
            preview_clear
            chafa --view-size "$(dimensions)" "$1"
            ;;
        esac
    else file "$FILE"
    fi
}

pdf_thumbnail()
{
    local target; target="$CACHE_DIR/$(hash_for "$FILE")"
    local cached_image="$target.png"
    if ! [ -f "$cached_image" ]
    then pdftoppm -png -singlefile "$FILE" "$target" -scale-to "$RATIO"
    fi >/dev/null
    echo "$cached_image"
}

audio_thumbnail()
{
    local cached_image
    cached_image="$CACHE_DIR/$(hash_for "$FILE").png"
    if ! [ -f "$cached_image" ]
    then ffmpeg -loglevel 0 -y -i "$FILE" -an -vcodec copy "$cached_image"
    fi >/dev/null
    echo "$cached_image"
}

video_thumbnail()
{
    local cached_image
    cached_image="$CACHE_DIR/$(hash_for "$FILE").png"
    if ! [ -f "$cached_image" ]
    then ffmpegthumbnailer -i "$FILE" -o "$cached_image" -s "$RATIO" -q 10
    fi >/dev/null
    echo "$cached_image"
}

comic_thumbnail()
{
    local cached_image
    cached_image="$CACHE_DIR/$(hash_for "$FILE").png"
    if ! [ -f "$cached_image" ]
    then unzip -p "$FILE" > "$cached_image"
    fi >/dev/null
    echo "$cached_image"
}

preview_dir()
{
    eza --color always --icons --group-directories-first -1 "$FILE"
}

preview_text()
{
    bat --color always    \
        --style numbers   \
        --pager never     \
        --wrap  never     \
        "$FILE"
}

preview_archive()
{
    atool -l -q "$FILE" | tail -n +2 | awk -F'   ' '{print $NF}'
}

hash_for()
{
    xxhsum "$1" | cut -d' ' -f 1
}

detect_terminal()
{
    local terminal="''${PREVIEW_FILE_AS:-default}"
    if    [ "''${TERM:-}" = "xterm-kitty" ] \
       || [ -n "''${KITTY_WINDOW_ID:-}" ]
    then terminal=kitty
    elif  [ "''${TERM_PROGRAM:-}" = "iTerm.app" ] \
       || [ "''${LC_TERMINAL:-}"  = "iTerm2"    ] \
       || [ -n "''${ITERM_SESSION_ID:-}" ]
    then terminal=iterm
    fi
    echo "$terminal"
}

dimensions()
{
    local dim
    dim="''${FZF_PREVIEW_COLUMNS:-}x''${FZF_PREVIEW_LINES:-}"
    if [ "$dim" = x ]
    then
        dim="$(stty size < /dev/tty | awk '{print $2 "x" $1}')"
    fi
    if [ -n "''${FZF_PREVIEW_COLUMNS:-}" ] && [ -n "''${FZF_PREVIEW_LINES:-}" ]
    then
        case "$(detect_terminal)" in
        kitty|kitty-*) dim="$((FZF_PREVIEW_COLUMNS + 2))x$FZF_PREVIEW_LINES" ;;
        iterm|iterm-*) ;;
        *) dim="$((FZF_PREVIEW_COLUMNS - 1))x$FZF_PREVIEW_LINES" ;;
        esac
    fi
    echo "$dim"
}

main "$@"
''
