{ lib, ... }:

let workEmail = lib.mkForce "shajra@groq.com";
in {
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
