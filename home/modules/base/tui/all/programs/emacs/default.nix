pkgs: {
  enable = true;

  extraPackages =
    epkgs: with epkgs; [
      vterm
    ];

  # REVISIT: 2026-01-27: Note emacs-macport doesn't have native compilation
  # https://github.com/railwaycat/homebrew-emacsmacport/issues/274
  # DESIGN: emacs-macport at least now builds on for Apple silicon
  # https://github.com/NixOS/nixpkgs/issues/127902

  # REVISIT: 2026-01-27: Periodically confirm matching the latest recommended
  # version.  https://github.com/doomemacs/doomemacs#prerequisites
  package = pkgs.emacs30;
}
