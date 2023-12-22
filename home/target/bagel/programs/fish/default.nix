pkgs:

let sft-tmux-script = pkgs.writeText "sft-tmux.scpt" ''
        set userName to system attribute "USER"
        tell application "iTerm"
            activate
            set newWindow to (create window with default profile)
            delay 0.2
            tell current session of newWindow
                write text "sft ssh " & userName & ¬
                    " --command \"tmux -CC new -A -s tmssh\""
            end tell
        end tell
    '';

in {
    functions = {
        sft-tunnel = {
            description = "Tunnel using ScaleFT";
            body = ''
                sft ssh --local-port-forward $argv[1]:localhost:$argv[1] (whoami)
            '';
        };
    };

    shellAliases = {
        tunnel-grafana = ''sft-tunnel 4000'';
        tunnel-nike    = ''sft-tunnel 8428'';
        tmux-cc = ''osascript ${sft-tmux-script}'';
    };
}
