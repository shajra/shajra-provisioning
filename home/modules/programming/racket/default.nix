{ build, ... }:

{
    imports = [ ../../ubiquity ];
    home.extraPackages = build.pkgs.lists.programming.racket;
}
