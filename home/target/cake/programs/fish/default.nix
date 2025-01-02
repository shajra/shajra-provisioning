pkgs:

let
    gmi = "${pkgs.lieer}/bin/gmi";
    notmuch = "${pkgs.notmuch}/bin/notmuch";
    rclone = "${pkgs.rclone}/bin/rclone";
in {
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
                sync-mail $argv && sync-drive $argv && sync-photos $argv
            '';
        };
    };

    shellAliases = {
      sync-drive  = ''${rclone} bisync google-tnks-public-drive:reference ~/doc/reference'';
      sync-photos = ''${rclone}   sync google-tnks-private-photos:media/by-month /srv/pictures/phones/sukant/'';
    };
}
