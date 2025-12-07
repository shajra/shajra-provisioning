{ build, ... }:

{
  imports = [
    ./packages
    ./theme
  ];
  home.stateVersion = "25.11";
  nix.package = build.infra.np.nixpkgs.system.nix;
}
