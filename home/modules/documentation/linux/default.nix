{ build, ... }:

{
    imports = [
        ../../ubiquity
        ../all
    ];
    home.extraPackages = build.pkgs.lists.documentation.linux;
    programs.texlive = import programs/texlive;
}
