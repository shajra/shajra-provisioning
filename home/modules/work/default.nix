{ lib, pkgs, ... }:

let workEmail = lib.mkForce "shajra@groq.com";
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
    programs.fish.functions.groq-rebase-mine = {
        description = "Rebase my local branches";
        body = ''
            jj git fetch && jj rebase -d head (
                jj branch list 'glob:shajra/*' \
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
                    remote_branches(exact:"head",   exact:"origin")   |
                    remote_branches(exact:"main",   exact:"origin")   |
                    remote_branches(exact:"master", exact:"origin")   |
                    remote_branches(exact:"trunk",  exact:"origin")   |
                    remote_branches(exact:"main",   exact:"upstream") |
                    remote_branches(exact:"master", exact:"upstream") |
                    remote_branches(exact:"trunk",  exact:"upstream") |
                    root()
                )
            '';
        };
    };
}
