config:

{
    extraConfig = ''
        #include "${config.home.homeDirectory}/.Xresources.dpi"
    '';

    properties = 
        (   import ./colors.nix) 
        // (import ./colors-light.nix)
        // {
            "Xcursor.theme"         = "Adwaita";
            "Xcursor.size"          = 32;
            # DESIGN: unset bindings with Control+j/k, otherwise a conflict
            "rofi.kb-accept-entry"  = "Control+m,Return,KP_Enter";
            "rofi.kb-remove-to-eol" = "";
            "rofi.kb-row-down"      = "Down,Control+j";
            "rofi.kb-row-up"        = "Up,Control+k";
        };
}
