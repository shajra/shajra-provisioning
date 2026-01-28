config: lib: pkgs:

{
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
