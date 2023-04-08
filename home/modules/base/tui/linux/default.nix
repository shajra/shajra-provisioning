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

    # DESIGN: Nixpkgs's Emacs has native compilation; no need for unstable
    #programs.emacs.package = pkgs.emacsUnstable;

    services.emacs = import services/emacs;

    xdg.mimeApps = import xdg/mimeApps;
}
