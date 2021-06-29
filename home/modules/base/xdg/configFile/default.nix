config: pkgs: sources:

let
    lorelei = (import sources.direnv-nix-lorelei).direnv-nix-lorelei;
in

{
    doom.source = ./doom;
    "stylish-haskell/config.yaml".source = stylish-haskell/config.yaml;
    "direnv/lib/nix-lorelei.sh".source = "${lorelei}/share/direnv-nix-lorelei/nix-lorelei.sh";
    "fish/set-universal.fish".onChange = import fish/onChange.nix config;
    "fish/set-universal.fish".source = fish/set-universal.fish;
    "fish/conf.d/direnv.fish".text = pkgs.callPackage fish/direnv.nix {
        cacheHome = config.xdg.cacheHome;
    };
}
