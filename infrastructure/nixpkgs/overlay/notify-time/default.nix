self: super:

let
    progName = "notify-time";
    meta.description = "Time a command and send a notification";
    isDarwin = super.stdenv.isDarwin;
    italics.open  = if isDarwin then "" else "<i>";
    italics.close = if isDarwin then "" else "</i>";
    success_sound = if isDarwin then "Blow" else "dialog-information";
    error_sound   = if isDarwin then "Submarine" else "dialog-error";
    command =
        if isDarwin
        then ''
            "${self.terminal-notifier}/bin/terminal-notifier" \
                -title "$summary" \
                -message "$body" \
                -activate net.kovidgoyal.kitty \
                -sound "$sound"
        ''
        else ''
            "${self.dunst}/bin/dunstify" \
                --appname notify-time \
                --icon "$icon" \
                --urgency "$urgency" \
                "$summary" \
                "$body"

            "${self.libcanberra-gtk3}/bin/canberra-gtk-play" \
                --id "$sound" \
                2>/dev/null
        '';
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
}
''
set -eu
set -o pipefail

NL="
"


main()
{
    local start="$SECONDS"
    set +e
    "$@"
    local exit_code="$?"
    set -e
    local end="$SECONDS"
    local duration="$((end - start))"
    local sound="${success_sound}"

    local summary="Command Done"

    # DESIGN exporting just to trick ShellCheck when compiling the Darwin
    # variant of this script (unused variables)
    export urgency=normal
    export icon=computer-symbolic.symbolic

    local body
    body="${italics.open}Command:${italics.close} $(pretty_command "$@")"
    body="${italics.open}Duration:${italics.close} $(pretty_duration "$duration")$NL$body"

    if [ "$exit_code" -gt "0" ]
    then
        summary="Command Failed"
        urgency=critical
        icon=computer-fail-symbolic.symbolic
        body="${italics.open}Exit code:${italics.close} $exit_code$NL$body"
        sound="${error_sound}"
    fi

    ${command}

    return "$exit_code"
}

pretty_command()
{
    for arg in "$@"; do
        if [[ $arg =~ [\"\ ] ]]; then
          arg=\"''${arg//\"/\\\"}\"
        fi
        echo -n "$arg "
    done
}

pretty_duration()
{
    local runtime="$1"
    local hours="$((runtime / 3600))"
    local remaining="$((runtime % 3600))"
    local minutes="$((remaining / 60))"
    local seconds="$((remaining % 60))"
    local pretty="''${seconds}s"
    if [ "$minutes" -gt 0 ] || [ "$hours" -gt 0 ]
    then pretty="''${minutes}m $pretty"
    fi
    if [ "$hours" -gt 0 ]
    then pretty="''${hours}h $pretty"
    fi
    echo "$pretty"
}


main "$@"
''
