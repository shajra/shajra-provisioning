config: pkgs: lib:

let

    gpg-connect-agent = "${config.programs.gpg.package}/bin/gpg-connect-agent";

in

{
    enable = true;

    functions = {
        # DESIGN: magic string to clear out Kitty; safe for other terminals
        # https://github.com/junegunn/fzf/issues/3228#issuecomment-1803402184
        __fzf_search_directory = {
            description = "Fuzzy find a file w/ Kitty image clear";
            body = ''
                _fzf_search_directory
                printf "\x1b_Ga=d,d=A\x1b\\"
            '';
        };

        # DESIGN: This disablement of DBUS_SESSION_BUS_ADDRESS is only needed
        # for gpg-agent's ssh-agent emulation with Gnome3 pin entry, which I
        # only stick with for Solarized theming.  Qt pin entry has the same
        # behavior out of the box without disabling DBus.  If using Qt, Home
        # Manager's services.gpg-agent.enableSshSupport would make the
        # updatestartuptty call.  We wouldn't need to call this function from
        # interactiveShellInit below.  Still, it's useful to have the
        # updatestartuptty call wrapped up as a function to grab back control.
        gpg-pinentry-claim = {
            description = "Set terminal to accept pinentry requests";
            body = ''
                if set -q SSH_CONNECTION
                    set --global --export PINENTRY_USER_DATA tty
                    DBUS_SESSION_BUS_ADDRESS=/dev/null \
                        ${gpg-connect-agent} updatestartuptty /bye
                else
                        ${gpg-connect-agent} updatestartuptty /bye
                end
            '';
        };
        system-info = {
            description = "A summary of system information";
            body = ''
                "${config.programs.macchina.package}/bin/macchina" --theme shajra
                # DESIGN: not messing with color_bars for now
                #"${pkgs.coreutils}/bin/cat" "${./color_bars.txt}"
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
                else if test "$snd" != ""; and test (string replace -r '\.' "" $snd) -ge $fst
                    __zoxide_zi $argv
                else
                    __zoxide_z $argv
                end
            '';
        };
        path-rebuild = {
            description = "Rebuild fish_user_paths";
            body = ''
                set --erase --universal fish_user_paths
                for d in ~/src/live/*/bin /opt/*/bin
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
                for doc in \
                        (fd "nix-.*[.]org" ~/src/shajra \
                        --type file \
                        --exclude '*-include.org' \
                        --exclude /nix-project/)
                    echo cp \
                        (string replace --regex '/shajra/[^/]+/' \
                            "/shajra/nix-project/" $doc) \
                        $doc
                    cp \
                        (string replace --regex '/shajra/[^/]+/' \
                            "/shajra/nix-project/" $doc) \
                        $doc
                end
            '';
        };
    };

    shellInit = ''
        set fzf_preview_dir_cmd  "${pkgs.preview-file}/bin/preview-file"
        set fzf_preview_file_cmd "${pkgs.preview-file}/bin/preview-file"
        set fzf_dir_opts --preview-window nowrap
    '';

    interactiveShellInit =
        let early = ''
                set -gx COLORTERM truecolor
                set -gx EDITOR vim
                set -gx LESS FRX
                fish_vi_key_bindings
                system-info
            '';
            late = lib.mkAfter ''
                umask 077

                # DESIGN: Rebinding to "j" for "jump" because base/gui/linux
                # will bind to "c" for "clipboard".
                bind \ej fzf-cd-widget

                # DESIGN: Rebinding PatrickF1/fzf.fish's history search, which
                # is nicer than the standard fzf function. Home Manager's fzf
                # module had a later precedence than the plugins of this module.
                bind \cr _fzf_search_history

                # DESIGN: Wrapping with hack for rendering images correctly.
                # Also, rebinding to not require extra Ctrl modifier.
                bind   \ef __fzf_search_directory
                bind \e\cf __fzf_search_directory

                if bind -M insert > /dev/null 2>&1
                    bind -M insert   \ej   fzf-cd-widget
                    bind -M insert   \cr  _fzf_search_history
                    bind -M insert   \ef __fzf_search_directory
                    bind -M insert \e\cf __fzf_search_directory
                end

                gpg-pinentry-claim > /dev/null
            '';
        in lib.mkMerge [ early late ];

    plugins = [
        {
            name = "fzf" ;
            src = pkgs.sources.fzf-fish;
        }
    ];

    shellAliases = {
        c = "bat";
        d = "devour";
        ec = "emacsclient -c -n";
        emacs-doom = "emacs --with-profile doom & disown";
        emacs-min = "emacs --with-profile min & disown";
        emacs-space = "emacs --with-profile space & disown";
        g = "git";
        ji = "__zoxide_zi";
        j = "zoxide-smart";
        l1 = "eza --icons --group-directories-first -1";
        la = "eza --icons --group-directories-first -lah";
        l = "eza --icons --group-directories-first";
        ll = "eza --icons --group-directories-first -l";
        lt = "eza --icons --group-directories-first --tree";
        m = "man";
        p = "${pkgs.jj-projects}/bin/jj-projects -b main -b user/shajra/next ~/src/shajra";
        unison = "unison -ui text";
        view = "vim -R";
        v = "vim";
    };
}
