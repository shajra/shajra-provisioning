config: pkgs: sources:

{
    doom.source = ./doom;
    "fish/completions/dunst-time.fish".source = fish/completions/dunst-time.fish;
    "fish/conf.d/direnv.fish".text = pkgs.callPackage fish/direnv.nix { cacheHome = config.xdg.cacheHome; };
    "fish/set-universal.fish".onChange = import fish/onChange.nix config;
    "fish/set-universal.fish".source = fish/set-universal.fish;
    "stylish-haskell/config.yaml".source = stylish-haskell/config.yaml;
}
