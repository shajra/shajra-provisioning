config:

{
    ".kshrc".text = import ksh/kshrc.nix config.home.homeDirectory;
    ".skhdrc".source = skhd/skhdrc;
    ".yabairc".source = yabai/yabairc;
    ".yabairc".executable = true;
    ".nix-channels".source = ./nix-channels;
}
