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

    # DESIGN: Allows VSCode to override settings, but everything will be lost
    # upon a home-manager switch.
    #
    # REVISIT: Might want to complicate logic to more easily catch changes or
    # maybe save them off as backups.
    #
    home.activation.removeVscodeMutableUserSettings =
        lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
            $DRY_RUN_CMD rm -f "${vscodeSettings}"
        '';

    home.activation.makeVscodeUserSettingsMutable =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD rm -f "${vscodeSettings}"
            $DRY_RUN_CMD cat \
              ${(pkgs.formats.json {}).generate
                  "vscode-settings.json"
                  config.programs.vscode.userSettings} \
              > "${vscodeSettings}"
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
