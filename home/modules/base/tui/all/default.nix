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
        home.file = import home/file config pkgs;

        programs.bash.enable = true;
        programs.bat.enable = true;
        programs.bat.config.theme = config.theme.external.bat.name;
        programs.bottom.enable = true;
        programs.bottom.settings.flags.color = config.theme.external.bottom.name;
        programs.btop.enable = true;
        programs.btop.settings.color_theme = config.theme.external.btop.name;
        programs.dircolors.enable = true;
        programs.dircolors.extraConfig = config.theme.external.dircolors.extraConfig;
        # DESIGN: Not using the Direnv module because Home Manager automatically
        # integrates Fish with Direnv in a way that's hard to override, and I
        # have my own preferred Direnv integration scripts for Fish.
        programs.direnv.enable = false;
        # DESIGN: eventually can move Lorelei back for Macs
        #programs.direnv-nix-lorelei.enable = true;
        programs.emacs = import programs/emacs pkgs;
        programs.feh.enable = true;
        programs.fish = import programs/fish config pkgs lib;
        programs.fzf.enable = true;
        programs.gh.enable = true;
        programs.gh-dash.enable = true;
        programs.git = import programs/git sources.delta;
        programs.gitui.enable = true;
        # DESIGN: not using Home-manager module because all it does it tie
        # the executable to dynamic configuration.
        programs.home-manager.enable = false;
        programs.htop.enable = true;
        programs.jq = import programs/jq;
        programs.jujutsu = import programs/jujutsu;
        programs.lesspipe.enable = true;
        programs.man.generateCaches = true;
        programs.neovim = import programs/neovim config pkgs;
        programs.readline = import programs/readline;
        programs.ripgrep.enable = true;
        programs.starship = import programs/starship;
        programs.tmux = import programs/tmux;
        programs.tealdeer.enable = true;
        programs.yazi.enable = true;
        programs.yazi.enableFishIntegration = true;
        programs.zoxide.enable = true;

        xdg.configFile = import xdg/configFile config pkgs;
    };
}
