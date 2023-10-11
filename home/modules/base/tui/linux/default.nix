{ pkgs, build, ... }:

let

    sources = pkgs.sources;
    module-lorelei = (import sources.direnv-nix-lorelei).direnv-nix-lorelei-home;

in

{
    imports = [
        ../../../ubiquity
        ../all
        # DESIGN: DEBUG: removing to understand infinite recursion
        #module-lorelei
    ];

    home.extraPackages = build.pkgs.lists.base.tui.linux;

    # DESIGN: DEBUG: removing to understand infinite recursion
    #programs.direnv-nix-lorelei.enable = true;

    # DESIGN: https://github.com/doomemacs/doomemacs#prerequisites
    # using emacs29 over emacs-unstable to hit Nixpkgs cache
    programs.emacs.package = pkgs.emacs29;

    services.emacs = import services/emacs;
    services.gpg-agent.enable = true;
    services.gpg-agent.enableSshSupport = true;

    xdg.mimeApps = import xdg/mimeApps;
}
