config: pkgs:

let

    clipmenu = "${config.services.clipmenu.package}/bin/clipmenu";
    fzf = "${config.programs.fzf.package}/bin/fzf";
    fzf-clip = pkgs.writers.writeDash "fzf-clip" ''
        ${fzf} --cycle --layout=reverse --border --height=90%
    '';
in

{
    functions = {
        "clipmenu-fzf" = {
            description = "Select a clip from clipmenu via fzf";
            body = ''
                set result (
                    CM_LAUNCHER="${fzf-clip}" \
                        CM_OUTPUT_CLIP=1 \
                        "${clipmenu}"
                )
                if [ -n "$result" ]
                    commandline --insert --current-token -- $result
                    commandline -f repaint
                end
            '';
        };
    };

    interactiveShellInit = ''
        bind \ec clipmenu-fzf
        if bind -M insert > /dev/null 2>&1
            bind -M insert \ec clipmenu-fzf
        end
    '';
}
