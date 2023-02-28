{ config, pkgs, lib, build, ... }:

let

    sources = pkgs.sources;
    # DESIGN: eventually lorelei will return...
    #module-lorelei = (import sources.direnv-nix-lorelei).direnv-nix-lorelei-home;
    extraPackages = [ pkgs.sqlite ] ++ build.pkgs.lists.base.tui.all;

in

{
    imports = [
        ../../../ubiquity
    ];

    options = {
        programs.macchina.package = lib.mkOption {
            type = lib.types.package;
            default = pkgs.macchina;
            description = ''Package of Macchina to use.'';
        };
    };

    config = {
        home.extraPackages = extraPackages;
        home.file = import home/file config;

        programs.bash.enable = true;
        programs.bottom = import programs/bottom config;
        programs.bat.config = { theme = config.theme.external.bat.name; };
        programs.bat.enable = true;
        programs.broot = import programs/broot config pkgs;
        programs.dircolors.enable = true;
        programs.dircolors.extraConfig = config.theme.external.dircolors.extraConfig;
        # DESIGN: Not using the Direnv module because Home Manager automatically
        # integrates Fish with Direnv in a way that's hard to override, and I
        # have my own preferred Direnv integration scripts for Fish.
        programs.direnv.enable = false;
        # DESIGN: eventually can move Lorelei back for Macs
        #programs.direnv-nix-lorelei.enable = true;
        programs.emacs = import programs/emacs;
        programs.feh.enable = true;
        programs.fish = import programs/fish config pkgs;
        programs.fzf.enable = true;
        programs.gh.enable = true;
        programs.git = import programs/git sources.delta;
        # DESIGN: not using Home-manager module because all it does it tie
        # the executable to dynamic configuration.
        programs.home-manager.enable = false;
        programs.htop.enable = true;
        programs.jq = import programs/jq;
        programs.lesspipe.enable = true;
        programs.man.generateCaches = true;
        programs.neovim = import programs/neovim pkgs;
        programs.nnn.enable = true;
        programs.readline = import programs/readline;
        programs.skim.enable = true;
        programs.starship = import programs/starship;
        programs.tealdeer.enable = true;
        programs.zoxide.enable = true;

        xdg.configFile = import xdg/configFile config pkgs;
    };
}
