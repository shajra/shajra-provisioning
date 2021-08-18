pkgs: colored_man-raw:

let

    # DESIGN: Make colors Solarized Light
    colored_man = pkgs.runCommand "colored_man" {} ''
        cp -r "${colored_man-raw}" "$out"
        substituteInPlace "$out/functions/cless.fish" \
            --replace '[38;5;31m'  '[36m' \
            --replace '[38;5;70m'  '[32m' \
            --replace '[38;5;220m' '[0m$reversed_ansi_code'
    '';

in

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

    plugins = [{
        name = "colored_man";
        src = colored_man;
    }];

    shellAliases = {
        c = "bat";
        emacs-doom = "emacs --with-profile doom & disown";
        emacs-min = "emacs --with-profile min & disown";
        emacs-space = "emacs --with-profile space & disown";
        g = "git";
        j = "pazi_cd";
        l1= "exa --icons --group-directories-first -1";
        la= "exa --icons --group-directories-first -lah";
        l = "exa --icons --group-directories-first";
        ll= "exa --icons --group-directories-first -l";
        t = "dunst-time";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
