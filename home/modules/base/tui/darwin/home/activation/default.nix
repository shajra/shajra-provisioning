config: lib: pkgs:

{
  # REVISIT: 2026-01-09: Fix has merged, commenting out workaround.
  # Remove dead code upon validating upstream fix works.
  # https://github.com/nix-community/home-manager/issues/1341
  # https://github.com/nix-darwin/nix-darwin/pull/1396
  #
  # copyApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   app_folder="Home Manager Apps"
  #   app_path="$(echo ~/Applications)/$app_folder"
  #   tmp_path="$(mktemp -dt "$app_folder.XXXXXXXXXX")" || exit 1
  #   find "$newGenPath/home-path/Applications" -type l \
  #           -exec readlink -f {} \; | \
  #       while read -r app
  #       do
  #         $DRY_RUN_CMD /usr/bin/osascript \
  #           -e "tell app \"Finder\"" \
  #           -e "make new alias file \
  #                   at POSIX file \"$tmp_path\" \
  #                   to POSIX file \"$app\"" \
  #           -e "set name of result to \"$(basename "$app")\"" \
  #           -e "end tell"
  #       done
  #   $DRY_RUN_CMD [ -e "$app_path" ] && rm -r "$app_path"
  #   $DRY_RUN_CMD mv "$tmp_path" "$app_path"
  # '';

  # DESIGN: Allows Karabiner to override settings, but we catch changes when
  # performing a home-manager switch.
  restoreImmutableKarabinerJson = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    SRC="${config.xdg.configFile."karabiner/karabiner.json".source}"
    DEST="${config.xdg.configHome}/karabiner/karabiner.json"
    if test -e "$DEST" && ! test -L "$DEST" \
        && test -e "$DEST.home-manager" \
        && test -L "$DEST.home-manager" \
        && "${pkgs.diffutils}/bin/diff" -q "$DEST" "$DEST.home-manager"
    then $DRY_RUN_CMD mv "$DEST.home-manager" "$DEST"
    fi
    if test -e "$DEST" && ! test -L "$DEST" \
        && "${pkgs.diffutils}/bin/diff" -q "$SRC" "$DEST"
    then $DRY_RUN_CMD rm "$DEST"
    fi
  '';

  makeKarabinerJsonSettingsMutable = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    DEST="${config.xdg.configHome}/karabiner/karabiner.json"
    $DRY_RUN_CMD mv "$DEST" "$DEST.home-manager"
    $DRY_RUN_CMD cat "$DEST.home-manager" > "$DEST"
  '';
}
