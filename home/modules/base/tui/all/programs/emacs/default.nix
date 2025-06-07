pkgs: {
    enable = true;

    extraPackages = epkgs: with epkgs; [
        vterm
    ];

    # REVISIT: emacs-macport was broken for M1
    # https://github.com/NixOS/nixpkgs/issues/127902
    # REVISIT: note emacs-macport doesn't have native compilation
    # https://github.com/railwaycat/homebrew-emacsmacport/issues/274

    # REVISIT: https://github.com/doomemacs/doomemacs#prerequisites
    # using emacs30 over emacs-unstable to hit Nixpkgs cache
    package = pkgs.emacs30;
}
