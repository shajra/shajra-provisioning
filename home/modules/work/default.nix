{ config, pkgs, ... }:

{
  programs.fish = {
    functions = {
      shajra-repo-fix = {
        description = "Fix configurations of non-work repositories";
        body = ''
          "${pkgs.fd}/bin/fd" '^[.]jj$' ~/src/shajra \
              --type directory --no-ignore-vcs --hidden \
              --exec "${pkgs.jujutsu}/bin/jj" --repository {//} \
                  config set --repo user.email ${config.shajra.email.dev}
          "${pkgs.fd}/bin/fd" '^[.]git$' ~/src/shajra \
              --type directory --no-ignore-vcs --hidden \
              --exec "${pkgs.git}/bin/git" -C {//} \
                  config user.email ${config.shajra.email.dev}
          "${pkgs.fd}/bin/fd" '^[.]git$' ~/src/shajra \
              --type directory --no-ignore-vcs --hidden \
              --exec "${pkgs.git}/bin/git" -C {//} \
                  config core.sshCommand 'ssh -i ~/.ssh/id_ed25519.shajra'
        '';
      };
      sailpoint-rebase-mine = {
        description = "Rebase my local branches";
        body = ''
          jj git fetch && jj rebase -d head@origin (
              jj bookmark list 'glob:shajra/*' \
                  -T 'if(!remote, "-b\n" ++ name ++ "\n")'
          )
        '';
      };
    };
    interactiveShellInit = ''
      set -gx AWS_PROFILE dev
    '';
  };

  programs.jujutsu.settings = {
    revset-aliases = {
      # DESIGN: https://jj-vcs.github.io/jj/latest/config/#set-of-immutable-commits
      #"immutable_heads()" =
      #  ''builtin_immutable_heads() | remote_bookmarks(exact:"release", exact:"origin")'';
    };
  };
}
