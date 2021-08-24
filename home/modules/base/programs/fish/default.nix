pkgs: sources:

let

    exa-preview = "${pkgs.exa}/bin/exa --icons --group-directories-first -1";

    man-colored = pkgs.runCommand "man-colored" {} ''
        cp -r "${sources."colored_man_pages.fish"}" "$out"
        chmod -R +w "$out"

        # DESIGN: Renaming to avoid infinite recursion when wrapping
        mv "$out/functions/man.fish" "$out/functions/man-colored.fish"
        substituteInPlace "$out/functions/man-colored.fish" \
            --replace 'function man'  'function man-colored' \

        # DESIGN: Make colors Solarized Light
        substituteInPlace "$out/functions/cless.fish" \
            --replace '[38;5;31m'  '[36m' \
            --replace '[38;5;70m'  '[32m' \
            --replace '[38;5;220m' '[0m$reversed_ansi_code'
    '';

in

{
    enable = true;

    functions = {
        man = {
            description = "Color man pages with a reasonable width";
            body = ''
                set --export MANWIDTH (
                    if [ $COLUMNS -gt 80 ]
                        echo 80
                    else
                        echo $COLUMNS
                    end
                )
                man-colored $argv
            '';
        };
        path-rebuild = {
            description = "Rebuild fish_user_paths";
            body = ''
                set --erase --universal fish_user_paths
                for d in ~/src/live/*/bin
                    fish_add_path --universal $d
                end
            '';
        };
        pull-all = {
            description = "Git pull all under ~/src/live";
            body = ''
                for d in ~/src/live/*
                    git -C "$d" pull
                end
            '';
        };
        bored = {
            description = "Things to run when bored";
            body = ''
                unison
                and nix-channel --update
                and pull-all
                and nix search --update-cache > /dev/null
            '';
        };
        org-refs = {
            description = "Report references found in Org files";
            body = ''
                rg --type org --only-matching --no-filename --no-line-number \
                        '\[\[([^\]]+)\]' --replace '$1' \
                    | sort -u \
                    | grep -v '^#\|^\./\|^\.\./\|https://\|file:'
            '';
        };
        org-nix-propagate = {
            description = "Copy nix.org from master project to rest";
            body = ''
                find ~/src/shajra \
                    -name nix.org \
                    \! -path "*/nix-project/*" \
                    -exec cp ~/src/shajra/nix-project/doc/nix.org {} \;
            '';
        };
    };

    shellInit = ''
        set fzf_preview_dir_cmd ${exa-preview}
        set fzf_preview_file_cmd "${pkgs.bat}/bin/bat"
    '';

    interactiveShellInit = ''
        umask 077
        set EDITOR vim
        fish_vi_key_bindings
        bind \ej fzf-cd-widget
        bind \ef _fzf_search_directory
        if bind -M insert > /dev/null 2>&1
            bind -M insert \ej fzf-cd-widget
            bind -M insert \ef _fzf_search_directory
        end
    '';

    plugins = [
        {
            name = "man-colored";
            src = man-colored;
        }
        {
            name = "fzf" ;
            src = sources."fzf.fish";
        }
    ];

    shellAliases = {
        c = "bat";
        emacs-doom = "emacs --with-profile doom & disown";
        emacs-min = "emacs --with-profile min & disown";
        emacs-space = "emacs --with-profile space & disown";
        g = "git";
        ji = "__zoxide_zi";
        j = "__zoxide_z";
        l1 = exa-preview;
        la = "exa --icons --group-directories-first -lah";
        l = "exa --icons --group-directories-first";
        ll = "exa --icons --group-directories-first -l";
        nnn = "nnn -C";
        m = "man";
        t = "dunst-time";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
