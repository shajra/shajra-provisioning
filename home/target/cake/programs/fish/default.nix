pkgs:

let
  gmi = "${pkgs.lieer}/bin/gmi";
  notmuch = "${pkgs.notmuch}/bin/notmuch";
  rclone = "${pkgs.rclone}/bin/rclone";
in
{
  functions = {
    sync-mail = {
      description = "Sync personal Gmail";
      body = ''
        pushd ~/var/mail/gmail
        ${gmi} sync && ${notmuch} new && ${gmi} sync
        popd
      '';
    };
    sync-all = {
      description = "Sync everything personal with Google";
      body = ''
           sync-mail            $argv \
        && sync-drive-reference $argv \
        && sync-drive-purchased $argv
      '';
    };
  };

  shellAliases = {
    sync-drive-reference = "${rclone} bisync google-tnks-public-drive:reference ~/doc/reference";
    sync-drive-purchased = "${rclone} bisync google-tnks-public-drive:purchased ~/doc/purchased";
  };
}
