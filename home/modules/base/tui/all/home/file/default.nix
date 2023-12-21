config:

let
    fish  = "${config.programs.fish.package}/bin/fish";
in

{
    ".kshrc".text = import ksh/kshrc.nix fish;
    ".nix-channels".source = ./nix-channels;
    ".haskeline".text = "editMode: Vi";
}
