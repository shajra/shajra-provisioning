config: pkgs: pkgs-unstable:

let

    format = pkgs.lib.colors.format "0xff%R%G%B";
    colors = pkgs.lib.colors.transformColors format config.theme.colors;

in {
    "yabai/yabairc".text = import yabai/yabairc.nix pkgs pkgs-unstable colors;
    "yabai/yabairc".executable = true;
}
