{ build, ... }:

{
  imports = [ ../../ubiquity ];
  home.extraPackages = build.pkgs.lists.documentation;
  programs.pandoc.enable = true;
}
