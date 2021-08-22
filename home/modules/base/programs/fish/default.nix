pkgs: sources:

let

    # DESIGN: Make colors Solarized Light
    colored_man = pkgs.runCommand "colored_man" {} ''
        cp -r "${sources."colored_man_pages.fish"}" "$out"
        substituteInPlace "$out/functions/cless.fish" \
            --replace '[38;5;31m'  '[36m' \
            --replace '[38;5;70m'  '[32m' \
            --replace '[38;5;220m' '[0m$reversed_ansi_code'
    '';

in

{
    enable = true;

    functions = {
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

    interactiveShellInit = ''
        umask 077
        fish_vi_key_bindings
        set EDITOR vim
    '';

    plugins = [
        {
            name = "colored_man";
            src = colored_man;
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
        l1= "exa --icons --group-directories-first -1";
        la= "exa --icons --group-directories-first -lah";
        l = "exa --icons --group-directories-first";
        ll= "exa --icons --group-directories-first -l";
        nnn = "nnn -C";
        t = "dunst-time";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
