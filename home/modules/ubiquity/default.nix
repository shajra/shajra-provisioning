{ build, ... }:

{
  imports = [
    ./packages
    ./theme
  ];
  home.stateVersion = "25.05";
  nix.package = build.infra.np.nixpkgs.system.nix;
}
