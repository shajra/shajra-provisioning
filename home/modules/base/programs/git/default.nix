delta:

{
    enable = true;

    userName = "Sukant Hajra";
    userEmail = "rrudbskr5g@snkmail.com";

    aliases = {
        a = "add";
        b = "branch";
        ca = "commit -a";
        c = "commit";
        cma = "commit -a -m";
        cm = "commit -m";
        co = "checkout";
        d = "diff --patience";
        f = "fetch";
        l = "log --date=local";
        lp = "log -p --date=local";
        lga = "log --color --graph --decorate"
            + " --pretty=format:'%Cred%h%Creset -%C(bold black)%d%Creset %s,"
            +     " %Cblue%an, %Cgreen%cd%Creset'"
            + " --abbrev-commit --date=local --all";
        lg = "log --color --graph --decorate"
            + " --pretty=format:'%Cred%h%Creset -%C(bold black)%d%Creset %s,"
            +     " %Cblue%an, %Cgreen%cd%Creset'"
            + " --abbrev-commit --date=local";
        p = "push";
        r = "reset";
        s = "status --short --branch";
        t = "tag";
        wipe = "clean -d -x -e .dir-locals.el -e .envrc -e .direnv";
        wipeall = "clean -d -x";
    };
    delta = {
        enable = true;
        options.features = "hoopoe";
    };
    extraConfig = {
        branch.autosetuprebase = "remote";
        color = {
            branch = "auto";
            diff = "auto";
            status = "auto";
        };
        "color \"branch\"" = {
            current = "yellow reverse";
            local = "yellow";
            remote = "green";
        };
        "color \"diff\"" = {
            meta = "yellow bold";
            frag = "magenta bold";
            old = "red";
        };
        "color \"status\"" = {
            added = "yellow";
            changed = "green";
            untracked = "cyan";
        };
        core = {
            editor = "vim";
            autocrlf = "input";
        };
        diff.submodule = "log";
        fetch.recurseSubmodules = true;
        github.user = "shajra";
        init.defaultBranch = "main";
        pull.rebase = true;
        push.default = "upstream";
        status.submodulesummary = true;
    };
    ignores = [
        # Macs
        ".DS_Store"

        # direnv
        #
        ".direnv/"
        ".envrc"

        # Vim
        #
        ".swo"
        ".swp"
        ".*.swo"
        ".*.swp"

        # Emacs
        #
        "\\#*#"
        ".#*"
        ".ensime"
        ".ensime_cache/"
        ".ensime_lucene"
        ".projectile"
        ".dir-locals.el"
        "*~"
        ".last-package-update-day"

        # IntelliJ
        #
        ".idea/"
        ".idea_modules"
        "*.iml"
        "*.ipr"
        "*.iws"

        # Eclipse
        #
        ".project"
        ".settings/"
        ".classpath"

        # Tag files
        #
        "cscope.out"
        "GPATH"
        "GRTAGS"
        "GTAGS"
        "tags"
        "TAGS"
        "TAGS.local"

        # SBT
        #
        "*.repl"

        # Haskell
        #
        ".hlint.yaml"

        # Python
        #
        ".ipynb_checkpoints/"
    ];
    includes = [{ path = "${delta}/themes.gitconfig"; }];
}
