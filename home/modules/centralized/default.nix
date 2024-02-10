{ build, ... }:

{
    imports = [ ../ubiquity ];
    home.extraPackages = build.pkgs.lists.centralized;
    programs.newsboat.enable = true;
}
