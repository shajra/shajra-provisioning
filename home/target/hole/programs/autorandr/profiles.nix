i3-dpi:

let
    DP-1-8 = import ./fingerprint.DP-1-8.nix;
    eDP-1  = import ./fingerprint.eDP-1.nix;
in

{
    home = {
        fingerprint = { inherit DP-1-8 eDP-1; };
        config = {
            DP-1-1.enable = false;
            DP-1.enable = false;
            HDMI-1.enable = false;
            HDMI-2.enable = false;
            eDP-1.enable = false;
            DP-1-8 = {
                enable = true;
                dpi = 150;
                primary = true;
                position = "0x0";
                mode = "2560x1440";
                rate = "59.95";
            };
        };
        hooks.preswitch = ''
            ${i3-dpi}/bin/i3-dpi 150
        '';
    };
    laptop = {
        fingerprint = { inherit eDP-1; };
        config = {
            DP-1-1.enable = false;
            DP-1.enable = false;
            HDMI-1.enable = false;
            HDMI-2.enable = false;
            DP-1-8.enable = false;
            eDP-1 = {
                enable = true;
                dpi = 235;
                primary = true;
                position = "0x0";
                mode = "3200x1800";
                rate = "59.98";
            };
        };
        hooks.preswitch = ''
            ${i3-dpi}/bin/i3-dpi 235
        '';
    };
}
