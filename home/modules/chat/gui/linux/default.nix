{ build, ... }:

{
    imports = [ ../../../ubiquity ];
    home.extraPackages = build.pkgs.lists.chat.gui.linux;
}
