pkgs: {
  enable = true;

  extraPackages =
    epkgs: with epkgs; [
      vterm
    ];

  # REVISIT: 2026-11-01: BLOCKED: emacs-macport lags the Emacs version Doom recommends
  # Nixpkgs now builds emacs-macport from jdtsmith/emacs-mac (with native
  # compilation, cached for Apple silicon), but only at Emacs 30.
  # https://github.com/jdtsmith/emacs-mac

  # REVISIT: 2026-11-01: REPEATING: Periodically confirm matching the latest recommended
  # version.  https://github.com/doomemacs/doomemacs#prerequisites
  package = pkgs.emacs31;
}
