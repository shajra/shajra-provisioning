{
  config,
  lib,
  pkgs,
  build,
  ...
}:

let

  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  vscodeSettingsBase = if isDarwin then "Library/Application Support" else "${config.xdg.configHome}";

  vscodeSettings = "${vscodeSettingsBase}/Code/User/settings.json";

  # REVISIT: 2024-11-07: Darwin build broken on unstable

in
{
  imports = [
    ../../../ubiquity
    ../../tui/all
  ];

  fonts.fontconfig.enable = true;

  # DESIGN: Allows VSCode to override settings, but we catch changes when
  # performing a home-manager switch.
  home.activation.restoreImmutableVscodeUserSettings =
    lib.hm.dag.entryBefore [ "checkLinkTargets" ]
      ''
        SRC="${
          (pkgs.formats.json { }).generate "vscode-settings.json"
            config.programs.vscode.profiles.default.userSettings
        }"
        DEST="${vscodeSettings}"
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

  home.activation.makeVscodeUserSettingsMutable = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    DEST="${vscodeSettings}"
    $DRY_RUN_CMD mv "$DEST" "$DEST.home-manager"
    $DRY_RUN_CMD cat "$DEST.home-manager" > "$DEST"
  '';

  home.extraPackages = build.pkgs.lists.base.gui;

  programs.alacritty = import programs/alacritty config pkgs;
  programs.fish = import programs/fish;
  programs.kitty = import programs/kitty config pkgs;
  programs.neovide = import programs/neovide config;
  programs.noti.enable = true;
  # REVISIT: 2024-12-10: even stable package broken for Darwin
  programs.sioyek.enable = !isDarwin;
  #programs.sioyek.package = sioyekPkg;
  programs.urxvt = import programs/urxvt config pkgs;
  programs.vscode = import programs/vscode config pkgs;
  programs.zathura.enable = true;

  xdg.configFile = import xdg/configFile config pkgs;
}
