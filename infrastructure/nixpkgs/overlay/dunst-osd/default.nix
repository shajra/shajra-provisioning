self: super:

let
    progName = "dunst-osd";
    meta.description = "Actions with transient notification to Dunst";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        coreutils
        dunst
        libcanberra-gtk3
        light
        ponymix
    ];
}
''
set -eu
set -o pipefail


. "${self.nix-project-lib.common}/share/nix-project/common.bash"


print_usage()
{
    cat - <<EOF
USAGE: ${progName} volume (up | down | mute-toggle)
       ${progName} brightness (up | down)

DESCRIPTION:

    Performs an action, and informs Dunst with a transient
    notification that functions like an On Screen Display (OSD).

EOF
}

main()
{
    case "$1" in
    volume)
        case "$2" in
        up)          change_volume increase 5% ;;
        down)        change_volume decrease 5% ;;
        mute-toggle) change_volume toggle      ;;
        *) die "unrecognized volume subcommand: $2" ;;
        esac
        ;;
    brightness)
        case "$2" in
        up)   change_brightness -A 1.0 ;;
        down) change_brightness -U 1.0 ;;
        *) die "unrecognized brightness subcommand: $2" ;;
        esac
        ;;
    *) die "unrecognized command: $1" ;;
    esac
}

change_volume()
{
    ponymix "$@"
    local setting; setting="$(ponymix get-volume)"
    if ponymix is-muted
    then dunstify \
        --timeout 1500 \
        --icon audio-volume-muted-symbolic.symbolic \
        --replace 9999 \
        --hints int:transient:1 \
        --hints "int:value:$setting" \
        "Muted" "<i>$setting%</i>"
    else
        intensity=low
        if [ "$setting" -gt 33 ]
        then intensity=medium
        fi
        if [ "$setting" -gt 66 ]
        then intensity=high
        fi
        dunstify \
            --timeout 1500 \
            --icon "audio-volume-$intensity-symbolic.symbolic" \
            --replace 9999 \
            --hints int:transient:1 \
            --hints "int:value:$setting" \
            "Volume" "<i>$setting%</i>"
    fi
    canberra-gtk-play --id audio-volume-change
}

change_brightness()
{
    light "$@"
    local setting; setting="$(light -G)"
    setting="$(printf '%.f\n' "$setting")"
    dunstify \
        --timeout 1500 \
        --icon display-brightness-symbolic.symbolic \
        --replace 9998 \
        --hints int:transient:1 \
        --hints "int:value:$setting" \
        "Backlight" "<i>$setting%</i>"
}


main "$@"
''
