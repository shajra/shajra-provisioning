{ build, ... }:

let

    hostname = "cake";
    user = build.config.provision.user."${hostname}".username;

in {
    nix.extraOptions = ''
        experimental-features = nix-command flakes
    '';
    nix.settings.auto-optimise-store = true;
    nix.settings.substituters = [
        "https://shajra.cachix.org"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://haskell-language-server.cachix.org"
        "https://nix-community.cachix.org"
    ];
    nix.settings.trusted-public-keys = [
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    nix.settings.trusted-users = [ "root" user ];

    # DESIGN: Don't think this is needed with flakes
    #nixpkgs.config = infra.np.config;
    #nixpkgs.overlays = infra.np.overlays;

    programs.fish.enable = true;
    time.timeZone = "America/Chicago";
}
