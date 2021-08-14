{
    enable = true;

    functions = {
        pull-all = {
            body = ''
                for d in ~/src/live/*
                    git -C "$d" pull
                end
            '';
        };
        bored = {
            body = ''
                nix-channel --update
                and unison
                and pull-all
            '';
        };
        org-refs = {
            body = ''
                rg --type org --only-matching --no-filename --no-line-number \
                        '\[\[([^\]]+)\]' --replace '$1' \
                    | sort -u \
                    | grep -v '^#\|^\./\|^\.\./\|https://\|file:'
            '';
        };
        org-nix-propagate = {
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

    shellAliases = {
        c = "bat";
        emacs-doom = "emacs --with-profile doom & disown";
        emacs-min = "emacs --with-profile min & disown";
        emacs-space = "emacs --with-profile space & disown";
        g = "git";
        j = "pazi_cd";
        l1= "lsd -1";
        la= "lsd -lah";
        ll= "lsd -l";
        l = "lsd";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
