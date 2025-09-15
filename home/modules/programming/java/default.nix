{ build, ... }:

{
  imports = [ ../../ubiquity ];
  home.extraPackages = build.pkgs.lists.programming.java;
  programs.java.enable = true;
}
