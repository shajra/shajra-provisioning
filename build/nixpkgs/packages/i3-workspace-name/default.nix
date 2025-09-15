{
  nix-project-lib,
  coreutils,
  i3,
  jq,
  rofi,
}:

let
  progName = "i3-workspace-name";
  meta.description = "Change I3 workspace name interactively";

  jqSwapTemplate = cond: op: ''
    .
    | to_entries as $workspaces
    | $workspaces[]
    | select(.value.focused==true) as $focused
    |   if ($focused.key ${cond})
        then
            $workspaces[]
            | select(.key == ($focused.key ${op} 1))
            | .value.name
        else empty
        end
  '';
  jqSwapPrev = jqSwapTemplate "> 0" "-";
  jqSwapNext = jqSwapTemplate "< ($workspaces | length) - 1" "+";

in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    envKeep = [
      "DISPLAY"
      "HOME"
      "I3SOCK"
      "XAUTHORITY"
    ];
    pathPackages = [
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
        case "''${1:-}" in
            prev|next) rename_space "jq_$1"   ;;
            [0-9]|10)  rename_space echo "$1" ;;
            *)         rename_space get_input ;;
        esac
    }

    rename_space()
    {
        local input; input="$("$@")"
        local trimmed_input; trimmed_input="$(trim "$input")"
        if [ -n "$trimmed_input" ]
        then smart_rename "$trimmed_input"
        fi
    }

    jq_prev() {
        i3-msg -t get_workspaces \
        | jq --raw-output '${jqSwapPrev}';
    }

    jq_next() {
        i3-msg -t get_workspaces \
        | jq --raw-output '${jqSwapNext}';
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
        local orig="$1"
        local new="$2"
        i3-msg "rename workspace \"$orig\" to \"$new\""
    }

    trim()
    {
        local suffix_trimmed="''${1%%*( )}"
        echo "''${suffix_trimmed##*( )}"
    }


    main "$@"
  ''
