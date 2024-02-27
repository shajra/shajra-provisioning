lib:

{
    enable = true;
    keyBindings = {
        normal = {
            ",v" = "hint links spawn --detach mpv {hint-url}";
            ",p" = "spawn --userscript qute-pass";
            "<F1>" = lib.mkMerge [
                "config-cycle tabs.show never always"
                "config-cycle statusbar.show in-mode always"
                "config-cycle scrolling.bar never always"
            ];
        };
    };
}
