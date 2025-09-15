lib:

{
  enable = true;
  keyBindings = {
    normal = {
      ";v" = "hint links spawn --detach mpv {hint-url}";
      ",v" = "spawn --detach mpv {url}";
      ",p" =
        "spawn --userscript qute-pass --unfiltered"
        + " --username-target secret"
        + " --username-pattern \"user: (.+)\"";
      ",P" = "spawn --userscript qute-pass --unfiltered" + " --password-only";
      "gj" = "tab-move +";
      "gk" = "tab-move -";
      "<Ctrl-j>" = "tab-move +";
      "<Ctrl-k>" = "tab-move -";
      "<Ctrl-=>" = "zoom-in";
      "<Ctrl-->" = "zoom-out";
      ">" = "config-cycle tabs.width 15% 20% 25% 30% 35% 40% 45% 50%";
      "<" = "config-cycle tabs.width 50% 45% 40% 35% 30% 25% 20% 15%";
      "tt" = lib.mkMerge [
        "config-cycle tabs.show      never   always"
        "config-cycle statusbar.show in-mode always"
        "config-cycle scrolling.bar  never   always"
      ];
      "tT" = lib.mkMerge [
        "config-cycle tabs.position  left   top"
      ];
    };
  };
  settings = {
    # DESIGN: may reduce monitor flashing when playing some videos
    content.webgl = false;
    qt.workarounds.disable_accelerated_2d_canvas = "always";
  };
}
