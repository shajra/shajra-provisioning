{
  config,
  lib,
  pkgs,
  ...
}:

let
  workCaDirectory = "${config.xdg.configHome}/nix";
  workCaRoot = "${workCaDirectory}/cisco-umbrella-root-ca.pem";
  workCaBundle = "${workCaDirectory}/ca-certificates.crt";
in

{
  home.activation.configureWorkTlsTrust = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD mkdir -p "${workCaDirectory}"

    if test -z "$DRY_RUN_CMD"
    then
      ROOT_TMP="${workCaRoot}.tmp"
      BUNDLE_TMP="${workCaBundle}.tmp"
      trap 'rm -f "$ROOT_TMP" "$BUNDLE_TMP"' EXIT

      /usr/bin/security find-certificate \
        -c "Cisco Umbrella Root CA" \
        -p /Library/Keychains/System.keychain \
        > "$ROOT_TMP"
      "${pkgs.openssl}/bin/openssl" x509 \
        -in "$ROOT_TMP" \
        -noout \
        -subject

      "${pkgs.coreutils}/bin/cat" \
        "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" \
        "$ROOT_TMP" \
        > "$BUNDLE_TMP"

      chmod 0644 "$ROOT_TMP" "$BUNDLE_TMP"
      mv "$ROOT_TMP" "${workCaRoot}"
      mv "$BUNDLE_TMP" "${workCaBundle}"
      trap - EXIT
    fi
  '';

  home.sessionVariables = {
    GIT_SSL_CAINFO = workCaBundle;
    NIX_SSL_CERT_FILE = workCaBundle;
    NODE_EXTRA_CA_CERTS = workCaRoot;
    SSL_CERT_FILE = workCaBundle;
  };

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

  programs.git.settings.http.sslCAInfo = workCaBundle;
}
