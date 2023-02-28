config: lib: nixpkgs:

let
    home = config.home.homeDirectory;
in

{
    # DESIGN: https://github.com/nix-community/home-manager/issues/1341
    copyApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        app_folder="Home Manager Apps"
        app_path="$(echo ~/Applications)/$app_folder"
        tmp_path="$(mktemp -dt "$app_folder.XXXXXXXXXX")" || exit 1
        find "$newGenPath/home-path/Applications" -type l \
                -exec readlink -f {} \; | \
            while read -r app
            do
              $DRY_RUN_CMD /usr/bin/osascript \
                -e "tell app \"Finder\"" \
                -e "make new alias file \
                        at POSIX file \"$tmp_path\" \
                        to POSIX file \"$app\"" \
                -e "set name of result to \"$(basename "$app")\"" \
                -e "end tell"
            done
        $DRY_RUN_CMD [ -e "$app_path" ] && rm -r "$app_path"
        $DRY_RUN_CMD mv "$tmp_path" "$app_path"
    '';
}
