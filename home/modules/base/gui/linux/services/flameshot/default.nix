config:

{
  enable = true;
  settings = {
    General = {
      disabledTrayIcon = true;
      showStartupLaunchMessage = false;
      savePath = "${config.home.homeDirectory}/tmp/screenshots";
      savePathFixed = false;
      startupLaunch = false;

      # REVISIT: 2026-08-01: BLOCKED: Workaround below
      # See https://github.com/nix-community/home-manager/issues/9201
      useX11LegacyScreenshot = true;
    };
  };
}
