{ build, ... }:

let

    hostname = "cake";
    user = build.config.provision.user."${hostname}".username;

in {
    imports = [ ../../../home/modules/ubiquity/theme/base.nix ];
    config = {
        nix.extraOptions = ''
            experimental-features = nix-command flakes
        '';
        nix.optimise.automatic = true;
        nix.settings.substituters = [
            "https://cache.garnix.io"
            "https://shajra.cachix.org"
            "https://nix-community.cachix.org"
            "https://haskell-language-server.cachix.org"
            "https://cache.iog.io"
        ];
        nix.settings.trusted-public-keys = [
            "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
            "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
            "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        ];
        nix.settings.trusted-users = [ "root" user ];

        # DESIGN: Don't think this is needed with flakes
        #nixpkgs.config = infra.np.config;
        #nixpkgs.overlays = infra.np.overlays;

        programs.fish.enable = true;
        time.timeZone = "America/Chicago";
    };
}
