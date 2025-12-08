{ build, ... }:

{
  imports = [
    ./packages
    ./theme
  ];

  # REVISIT: Holding back while churn from targets.darwin.{copyApps,linkApps}
  # settles downs.
  home.stateVersion = "25.05";

  nix.package = build.infra.np.nixpkgs.system.nix;
}
