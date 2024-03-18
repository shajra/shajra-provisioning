{ build, ... }:

{
    imports = [
        ../../../ubiquity
        ../all
    ];

    home.extraPackages = build.pkgs.lists.programming.c.linux;
}
