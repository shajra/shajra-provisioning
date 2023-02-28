config: pkgs:

let
    kitty = "${config.programs.kitty.package}/bin/kitty";
    jq    = "${pkgs.jq}/bin/jq";
in

{
    ".skhdrc".text = import skhd/skhdrc.nix kitty jq;
    ".yabairc".source = yabai/yabairc;
    ".yabairc".executable = true;
}
