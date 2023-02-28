self: super:

let

    progName = "fzf-preview";
    meta.description = "previewing tailored to FZF";

in

# IDEA: This script is dead/unfinished code based on
# https://gitlab.com/sourplum/dotfiles.
#
# I just didn't want to delete it just yet.  It attempts to use draw images with
# Kitty.  Unfortunately, it has two problems.  The placement of the image is
# naive.  Also, it doesn't clear out the drawn image when the preview box
# closes.
#
# Both of these problems I believe should be addressed in some part by FZF,
# which I believe should:
#
# - calculate the top-left corner of the image relative to the full terminal and
#   send that as another pair of environment variables
#
# - make another hook for external programs (like Kitty) to clear out the
#   screen, which can be configured as another environment variable.
#
self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        atool
        bat
        coreutils
        exa
        ffmpeg
        ffmpegthumbnailer
        file
        i3
        kitty
        ncurses
        poppler_utils
    ];
}
''
set -eu
set -o pipefail


SCREEN_LINES="$(tput lines)"
SCREEN_COLS="$(tput cols)"
Y="$(( (SCREEN_LINES - FZF_PREVIEW_LINES) / 2 + 2))"
X="$(( FZF_PREVIEW_COLUMNS  + 8 ))"
echo "$(date): (x=$X, y=$Y)" > ~/debug.fzf.log
echo "$(date): (lines=$SCREEN_LINES, cols=$SCREEN_COLS)" > ~/debug.fzf.log
echo "$(date): (fzf-lines=$FZF_PREVIEW_LINES, fzf-cols=$FZF_PREVIEW_COLUMNS)" > ~/debug.fzf.log
echo "$(date): (fzf-lines=$FZF_OUTER_LINES, fzf-cols=$FZF_OUTER_COLUMNS)" > ~/debug.fzf.log
WIDTH="$((  FZF_PREVIEW_COLUMNS  + 0 ))"
HEIGHT="$(( FZF_PREVIEW_LINES    + 0 ))"
TMP=~/.cache/"${progName}"
CACHEFILE="$TMP/$(echo "$1" | base64).png"


main()
{
    previewclear
    maketemp
    mimetype=$(file -b --mime-type "$1")

    case $mimetype in
    application/pdf)
        pdf "$1"
        ;;
    application/zip | application/x-tar | *rar | application/gzip )
        archive "$1"
        ;;
    audio/*)
        audio "$1"
        ;;
    image/*)
        image "$1"
        ;;
    text/*)
        text "$1"
        ;;
    video/*)
        video "$1"
        ;;
    inode/directory)
        if [ -z "''${1##*/..*}" ]
        then echo
        else exa -Fa --color always "$1"
        fi
        ;;
    *)
        text "$1"
        ;;
    esac
}

maketemp()
{
    if ! [ -d "$TMP" ]
    then mkdir -p "$TMP"
    fi
}

previewclear()
{
    kitty +kitten icat --transfer-mode=file --silent --clear
}

text()
{
    bat --pager=never --wrap never --style="changes" --color="always" "$1" -p
}

archive()
{
    atool -l -q "$1" | tail -n +2 | awk -F'   ' '{print $NF}'
}

draw()
{
    kitty +kitten icat \
        --transfer-mode=file \
        --silent \
        --place="''${WIDTH}x''${HEIGHT}@''${X}x''${Y}" \
        --z-index=-1 \
        "$1"
}

image()
{
    draw "$1"
}

# https://ffmpeg.org/ffmpeg-filters.html#showspectrum-1
audio()
{
    if ! [ -f "$CACHEFILE" ]
    then ffmpeg -loglevel 0 -y -i "$1" \
        -lavfi "showspectrumpic=s=hd480:legend=0:gain=5:color=intensity" \
        "$CACHEFILE"
    fi
    draw "$CACHEFILE"
}

video()
{
    if ! [ -f "$CACHEFILE" ]
    then ffmpegthumbnailer -i "$1" -o "$CACHEFILE" -s 1024 -q 10
    fi
    draw "$CACHEFILE"
}

pdf()
{
    if ! [ -f "$CACHEFILE.png" ]
    then pdftoppm -png -singlefile "$1" "$CACHEFILE" -scale-to 1024
    fi
    draw "$CACHEFILE.png"
}


main "$1"
''
