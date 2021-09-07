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
            "Xcursor.size"          = 48;
        };
}
