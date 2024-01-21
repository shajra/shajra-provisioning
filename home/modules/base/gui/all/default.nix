{ config, lib, pkgs, build, ... }:

let

    vscodeSettingsBase =
        if pkgs.stdenv.hostPlatform.isDarwin
        then "Library/Application Support"
        else "${config.xdg.configHome}";

    vscodeSettings = "${vscodeSettingsBase}/Code/User/settings.json";

in {
    imports = [
        ../../../ubiquity
        ../../tui/all
    ];

    fonts.fontconfig.enable = true;

    # DESIGN: Allows VSCode to override settings, but we catch changes when
    # performing a home-manager switch.
    home.activation.restoreImmutableVscodeUserSettings =
        lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
            DEST="${vscodeSettings}"
            if test -e "$DEST" && test -e "$DEST.home-manager" \
                && "${pkgs.diffutils}/bin/diff" -q "$DEST" "$DEST.home-manager"
            then $DRY_RUN_CMD mv "$DEST.home-manager" "$DEST"
            fi
        '';

    home.activation.makeVscodeUserSettingsMutable =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            DEST="${vscodeSettings}"
            $DRY_RUN_CMD mv "$DEST" "$DEST.home-manager"
            $DRY_RUN_CMD cat "$DEST.home-manager" > "$DEST"
        '';

    home.extraPackages = build.pkgs.lists.base.gui.all;

    programs.alacritty = import programs/alacritty config pkgs;
    programs.fish = import programs/fish;
    programs.kitty = import programs/kitty config pkgs;
    programs.noti.enable = true;
    programs.urxvt = import programs/urxvt config pkgs;
    programs.vscode = import programs/vscode config pkgs;

    xdg.configFile = import xdg/configFile config pkgs;
}
