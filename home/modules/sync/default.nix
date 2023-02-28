{ build, ... }:

{
    imports = [ ../ubiquity ];
    home.extraPackages = build.pkgs.lists.sync;
}
