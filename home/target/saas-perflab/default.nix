{
  config,
  #pkgs,
  #lib,
  build,
  ...
}:

let
  userConfig = build.config.provision.user;
  hostname = "saas-perflab";
in

{
  imports = [
    ../../modules/base/tui/linux

    ../../modules/programming/c
    ../../modules/programming/general
    ../../modules/programming/shell
    ../../modules/programming/web
  ];

  home.file = import home/file config;
  home.homeDirectory = userConfig."${hostname}".homeDirectory;
  home.username = userConfig."${hostname}".username;

  programs.fish = import programs/fish config;
  #programs.i3status-rust = import programs/i3status-rust config pkgs;

  #services.gammastep.enable = lib.mkForce false;

  targets.genericLinux.enable = true;

  xdg.configFile = import xdg/configFile config;

  #xsession.windowManager.i3 = import xsession/windowManager/i3 config lib;
}
