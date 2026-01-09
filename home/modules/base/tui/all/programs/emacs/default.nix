pkgs: {
  enable = true;

  extraPackages =
    epkgs: with epkgs; [
      vterm
    ];

  # REVISIT: 2026-01-09: Note emacs-macport doesn't have native compilation
  # https://github.com/railwaycat/homebrew-emacsmacport/issues/274
  # REVISIT: emacs-macport was broken for M1
  # https://github.com/NixOS/nixpkgs/issues/127902

  # REVISIT: 2026-01-09: Periodically confirm matching the latest recommended
  # version.  https://github.com/doomemacs/doomemacs#prerequisites
  package = pkgs.emacs30;
}
