pkgs: {
    enable = true;

    extraPackages = epkgs: with epkgs; [
        # DESIGN: 2024-08-10: Stopped using emacsql and emacsql-sqlite.  The
        # latter had a broken build, but it seems Doom is doing okay without
        # these dependencies (starting from scratch with a fresh .local).
        #emacsql
        #emacsql-sqlite
        vterm
    ];

    # REVISIT: emacs-macport was broken for M1
    # https://github.com/NixOS/nixpkgs/issues/127902
    # REVISIT: note emacs-macport doesn't have native compilation
    # https://github.com/railwaycat/homebrew-emacsmacport/issues/274

    # DESIGN: https://github.com/doomemacs/doomemacs#prerequisites
    # using emacs30 over emacs-unstable to hit Nixpkgs cache
    package = pkgs.emacs30;
}
