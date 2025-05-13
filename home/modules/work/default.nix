{ lib, pkgs, ... }:

let workEmail = lib.mkForce "sukant@imandra.ai";
in {
    programs.fish.functions.shajra-repo-fix = {
        description = "Fix configurations of non-work repositories";
        body = ''
            "${pkgs.fd}/bin/fd" '^[.]jj$' ~/src/shajra \
                --type directory --no-ignore-vcs --hidden \
                --exec "${pkgs.jujutsu}/bin/jj" --repository {//} \
                    config set --repo user.email dev.sukant@hajra.xyz
            "${pkgs.fd}/bin/fd" '^[.]git$' ~/src/shajra \
                --type directory --no-ignore-vcs --hidden \
                --exec "${pkgs.git}/bin/git" -C {//} \
                    config user.email dev.sukant@hajra.xyz
        '';
    };
    programs.fish.functions.imandra-web-make = {
        description = "Make an imandra-web target in a new Kitty tab";
        body = ''
            set src_dir "$HOME/src/work/imandra-web"
            set title $argv[1]
            "${pkgs.kitty}/bin/kitty" @ launch \
                --type=tab \
                --title="$title" \
                "${pkgs.direnv}/bin/direnv" exec "$src_dir" \
                "${pkgs.gnumake}/bin/make" --directory="$src_dir" $argv[2..-1]

        '';
    };
    programs.fish.functions.imandra-web-make-all = {
        description = "Make all imandra-web targets in a new Kitty tab";
        body = ''
            if test (count $argv) -gt 0
                set service $argv[1]
            else
                set service universe
            end
            imandra-web-make         db cloud-sql-proxy-dev
            imandra-web-make static  watch-static
            imandra-web-make build   watch-build
            imandra-web-make service dev "$service" watch-server
            "${pkgs.kitty}/bin/kitty" @ focus-tab --match index:0
        '';
    };
    programs.fish.functions.imandra-rebase-mine = {
        description = "Rebase my local branches";
        body = ''
            jj git fetch && jj rebase -d head@origin (
                jj bookmark list 'glob:shajra/*' \
                    -T 'if(!remote, "-b\n" ++ name ++ "\n")'
            )
        '';
    };
    programs.git.userEmail = workEmail;
    programs.jujutsu.settings = {
        user.email = workEmail;
        revset-aliases = {
            "trunk()" = ''
                latest(
                    remote_bookmarks(exact:"head",   exact:"origin")   |
                    remote_bookmarks(exact:"main",   exact:"origin")   |
                    remote_bookmarks(exact:"master", exact:"origin")   |
                    remote_bookmarks(exact:"trunk",  exact:"origin")   |
                    remote_bookmarks(exact:"main",   exact:"upstream") |
                    remote_bookmarks(exact:"master", exact:"upstream") |
                    remote_bookmarks(exact:"trunk",  exact:"upstream") |
                    root()
                )
            '';
        };
    };
}
