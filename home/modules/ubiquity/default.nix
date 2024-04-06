{ build, ... }:

{
    imports = [
        ./packages
        ./theme
    ];
    home.stateVersion = "23.11";
    nix.package = build.infra.np.nixpkgs.system.nix;
}
