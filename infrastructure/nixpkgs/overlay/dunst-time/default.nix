self: super:

let
    progName = "dunst-time";
    meta.description = "time a command and send a notification to Dunst";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
}
''
set -eu
set -o pipefail


main()
{
    local start="$SECONDS"
    set +e
    "$@"
    local exit_code="$?"
    set -e
    local end="$SECONDS"
    local duration="$((end - start))"

    local summary="Command Done"
    local urgency=normal
    local icon=computer-symbolic.symbolic
    local body
    body="<i>Command:</i> $(pretty_command "$@")"
    body="<i>Duration:</i> $(pretty_duration "$duration")\n$body"

    if [ "$exit_code" -gt "0" ]
    then
        summary="Command Failed"
        urgency=critical
        icon=computer-fail-symbolic.symbolic
        body="<i>Exit code:</i> $exit_code\n$body"
    fi

    "${self.dunst}/bin/dunstify" \
        --appname dunst-time \
        --icon "$icon" \
        --urgency "$urgency" \
        "$summary" \
        "$body"

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
