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

    # DESIGN: Emacs 29+ (emacsUnstable) isn't supported
    # Emacs 28 is now standard in nixpkgs-unstable
    # https://github.com/doomemacs/doomemacs#prerequisites
    #programs.emacs.package = pkgs.emacsUnstable;

    services.emacs = import services/emacs;
    services.gpg-agent.enable = true;
    services.gpg-agent.enableSshSupport = true;

    xdg.mimeApps = import xdg/mimeApps;
}
