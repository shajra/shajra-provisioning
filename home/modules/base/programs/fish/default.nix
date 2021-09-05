pkgs: sources: isDarwin:

let

    fzf-preview-dir = pkgs.writers.writeDash "fzf-preview-dir" ''
        "${pkgs.exa}/bin/exa" --color always \
                --icons --group-directories-first -1 \
            | "${pkgs.coreutils}/bin/head" -300
    '';

    fzf-preview-file = pkgs.writers.writeDash "fzf-preview-dir" ''
        "${pkgs.bat}/bin/bat" --color always --style numbers --wrap never \
            --line-range :300
    '';

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

    linuxInteractiveShellInit = ''
        bind \ej fzf-cd-widget
        bind \ef _fzf_search_directory
        if bind -M insert > /dev/null 2>&1
            bind -M insert \ej fzf-cd-widget
            bind -M insert \ef _fzf_search_directory
        end
    '';

    darwinInteractiveShellInit = ''
        # NOTE: these keyscodes are mysterious.  Use "fish_key_reader -c" to
        # discover what these codes should be.  They should be same as bindings
        # for Linux, but with GUI instead of Alt for Darwin (because skhd is
        # keybound heavily to Alt).
        bind \e\[101\;9u  edit_command_buffer            # GUI+e
        bind \e\[46\;9u   history-token-search-backward  # GUI+.
        bind \e\[108\;9u  __fish_list_current_token      # GUI+l
        bind \e\[104\;9u  __fish_man_page                # GUI+h
        bind \e\[112\;9u  __fish_paginate                # GUI+p
        bind \e\[115\;9u  __fish_prepend_sudo            # GUI+s
        bind \e\[111\;9u  __fish_preview_current_file    # GUI+o
        bind \e\[119\;9u  __fish_whatis_current_token    # GUI+w
        bind \e\[106\;9u fzf-cd-widget                   # GUI+j
        bind \e\[102\;9u _fzf_search_directory           # GUI+f
        if bind -M insert > /dev/null 2>&1
            bind -M insert \e\[101\;9u  edit_command_buffer
            bind -M insert \e\[46\;9u   history-token-search-backward
            bind -M insert \e\[108\;9u  __fish_list_current_token
            bind -M insert \e\[104\;9u  __fish_man_page
            bind -M insert \e\[112\;9u  __fish_paginate
            bind -M insert \e\[115\;9u  __fish_prepend_sudo
            bind -M insert \e\[111\;9u  __fish_preview_current_file
            bind -M insert \e\[119\;9u  __fish_whatis_current_token
            bind -M insert \e\[106\;9u fzf-cd-widget
            bind -M insert \e\[102\;9u _fzf_search_directory
        end
    '';

in

{
    enable = true;

    functions = {
        git-status = {
            description = "Jump into a deep directory with Broot";
            body = ''
                if set -q argv[1]
                    set targets $argv
                else
                    set targets ~/src/"${if isDarwin then "work" else "shajra"}"
                end
                for d in (fd --type d --hidden --glob .git $targets)
                    pushd $d
                    cd ..
                    starship prompt
                    popd
                end | sort | grep --color=never ' on '
            '';
        };
        broot-dir = {
            description = "Jump into a deep directory with Broot";
            body = ''
                if set -q argv[1]
                    br --only-folders --cmd "$argv[1] cd"
                else
                    br --only-folders
                end
            '';
        };
        broot-tree = {
            description = "Directory tree with Broot";
            body = ''
                br -c :pt $argv
            '';
        };
        zoxide-smart = {
            description = "Autojump if obvious, else fuzzy search";
            body = ''
                zoxide query --exclude (__zoxide_pwd) --list --score $argv \
                    | head -2 \
                    | awk '{print $1;}' \
                    | begin read fst; read snd; end
                if test "$fst" = ""
                    echo Sorry, No match >&2
                    __zoxide_zi
                else if test "$snd" != ""; and test (expr $snd \* 10) -ge $fst
                    __zoxide_zi $argv
                else
                    __zoxide_z $argv
                end
            '';
        };
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
        set fzf_preview_dir_cmd  "${fzf-preview-dir}
        set fzf_preview_file_cmd "${fzf-preview-file}
        set fzf_dir_opts --preview-window nowrap
    '';

    interactiveShellInit = ''
        umask 077
        set EDITOR vim
        fish_vi_key_bindings
        ${
            if isDarwin
            then darwinInteractiveShellInit
            else linuxInteractiveShellInit
        }
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
        brd = "broot-dir";
        brt = "broot-tree";
        c = "bat";
        ec = "emacsclient -c -n";
        emacs-doom = "emacs --with-profile doom & disown";
        emacs-min = "emacs --with-profile min & disown";
        emacs-space = "emacs --with-profile space & disown";
        g = "git";
        ji = "__zoxide_zi";
        j = "zoxide-smart";
        l1 = "exa --icons --group-directories-first -1";
        la = "exa --icons --group-directories-first -lah";
        l = "exa --icons --group-directories-first";
        ll = "exa --icons --group-directories-first -l";
        lt = "exa --icons --group-directories-first --tree";
        nnn = "nnn -C";
        ${if isDarwin then "mm" else null} = "${pkgs.m-cli}/bin/m";
        m = "man";
        s = "git-status";
        t = "notify-time";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
