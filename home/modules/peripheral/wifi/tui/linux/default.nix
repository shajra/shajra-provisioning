{ build, ... }:

{
    imports = [ ../../../../ubiquity ];
    home.extraPackages = build.pkgs.lists.peripheral.wifi.tui.linux;
}
