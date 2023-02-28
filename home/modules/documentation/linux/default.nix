{ build, ... }:

{
    imports = [ ../../ubiquity ];
    home.extraPackages = build.pkgs.lists.documentation.linux;
    programs.texlive = import programs/texlive;
}
