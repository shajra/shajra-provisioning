{ build, ... }:

{
  imports = [
    ./packages
    ./theme
  ];

  home.stateVersion = "26.05";

  nix.package = build.infra.np.nixpkgs.system.nix;
}
