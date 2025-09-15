{ build, config, ... }:

{
  imports = [ ../ubiquity ];
  home.extraPackages = build.pkgs.lists.centralized;
  programs.newsboat.enable = true;
  programs.rofi.pass.enable = true;
  programs.rofi.pass.stores = [ "${config.home.homeDirectory}/src/live/password-store" ];
}
