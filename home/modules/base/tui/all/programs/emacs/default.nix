pkgs: {
    enable = true;

    extraPackages = epkgs: with epkgs; [
        emacsql
        emacsql-sqlite
        vterm
    ];

    # DESIGN: https://github.com/doomemacs/doomemacs#prerequisites
    # using emacs29 over emacs-unstable to hit Nixpkgs cache
    package = pkgs.emacs29;
}
