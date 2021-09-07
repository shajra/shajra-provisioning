config: pkgs:

let
    fish  = "${config.programs.fish.package}/bin/fish";
    kitty = "${pkgs.kitty}/bin/kitty";
    jq    = "${pkgs.jq}/bin/jq";
in

{
    ".kshrc".text = import ksh/kshrc.nix fish;
    ".skhdrc".text = import skhd/skhdrc.nix kitty jq;
    ".yabairc".source = yabai/yabairc;
    ".yabairc".executable = true;
    ".nix-channels".source = ./nix-channels;
}
