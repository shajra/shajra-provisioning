{ build, ... }:

{
  imports = [ ../../ubiquity ];
  home.extraPackages = build.pkgs.lists.audio.tui;
  programs.ncmpcpp.enable = true;
}
