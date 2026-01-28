{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../../../ubiquity
    ../all
  ];

  home.activation = import home/activation config lib pkgs;

  # REVISIT: DARWIN: 2026-01-09: Nix-darwin broke
  # nixpkgs-unstable upgraded to Fish 4.3, while Nix-darwin seems to only work
  # with 4.2.
  programs.fish.package = build.infra.np.nixpkgs.system.fish;

  xdg.configFile = import xdg/configFile;
}
