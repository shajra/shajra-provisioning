{ config, lib, pkgs, ... }:

let
    build = import ../../.. {};
    infra = build.infra;
    sources = build.sources;
    module-lorelei = (import sources.direnv-nix-lorelei).direnv-nix-lorelei-home;
    prunedPkgs = builtins.removeAttrs build.pkgs [
        "emacsGcc"  # DESIGN: prebuilt, but passed in programs.emacs.package
    ];
    selectedPkgs =
        # DESIGN: undoes recurseIntoAttrs to pass validation
        builtins.filter lib.isDerivation (builtins.attrValues prunedPkgs);
in
{
    imports = [ module-lorelei ];

    fonts.fontconfig.enable = true;

    home.file = import home/file config.home.homeDirectory;
    home.packages = selectedPkgs;
    home.stateVersion = "21.05";

    nixpkgs.config = infra.np.config;
    nixpkgs.overlays = infra.np.overlays;

    programs.alacritty = import programs/alacritty;
    programs.bash.enable = true;
    programs.bat.config = { theme = "Solarized (light)"; };
    programs.bat.enable = true;
    programs.broot = import programs/broot;
    programs.dircolors.enable = true;
    programs.dircolors.extraConfig = builtins.readFile "${sources.dircolors-solarized}/dircolors.ansi-light";
    programs.direnv.enableFishIntegration = false;
    programs.direnv.enable = true;
    programs.direnv-nix-lorelei.enable = true;
    programs.emacs = import programs/emacs pkgs;
    programs.feh.enable = true;
    programs.fish = import programs/fish pkgs sources build.infra.isDarwin;
    programs.fzf.enable = true;
    programs.gh.enable = true;
    programs.git = import programs/git sources.delta;
    programs.home-manager.enable = true;
    programs.htop.enable = true;
    programs.jq.enable = true;
    programs.kitty = import programs/kitty pkgs;
    programs.lesspipe.enable = true;
    programs.man.generateCaches = true;
    programs.ncmpcpp.enable = true;
    programs.neovim = import programs/neovim pkgs;
    programs.newsboat.enable = true;
    programs.noti.enable = true;
    programs.readline = import programs/readline;
    programs.skim.enable = true;
    programs.starship = import programs/starship;
    programs.urxvt.enable = true;
    programs.zoxide.enable = true;

    xdg.configFile = import xdg/configFile config pkgs sources;
}
