{ config, lib, pkgs, ... }:

let
    build = import ../../.. {};
    infra = build.infra;
    sources = build.sources;
    selectedPkgs =
        # DESIGN: undoes recurseIntoAttrs to pass validation
        builtins.filter lib.isDerivation (builtins.attrValues build.pkgs);
in

{
    fonts.fontconfig.enable = true;

    home.file = import home/file config.home.homeDirectory;
    home.packages = selectedPkgs;
    home.stateVersion = "21.05";

    nixpkgs.config = infra.np.config;
    nixpkgs.overlays = infra.np.overlays;

    programs.alacritty = import programs/alacritty;
    programs.bash.enable = true;
    programs.dircolors.enable = true;
    programs.direnv.enable = true;
    programs.direnv.enableFishIntegration = false;
    programs.feh.enable = true;
    programs.fish = import programs/fish;
    programs.git = import programs/git sources.delta;
    programs.home-manager.enable = true;
    programs.htop.enable = true;
    programs.jq.enable = true;
    programs.kitty = import programs/kitty pkgs;
    programs.man.generateCaches = true;
    programs.ncmpcpp.enable = true;
    programs.starship = import programs/starship;
    programs.urxvt.enable = true;
    programs.neovim = import programs/neovim pkgs;

    xdg.configFile = import xdg/configFile config pkgs sources;
}
