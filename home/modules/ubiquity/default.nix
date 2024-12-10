{ build, ... }:

{
    imports = [
        ./packages
        ./theme
    ];
    home.stateVersion = "24.11";
    nix.package = build.infra.np.nixpkgs.system.nix;
}
