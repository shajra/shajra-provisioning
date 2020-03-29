let

    sources = import ../sources.nix;
    pkgs = import sources."nixos-stable" {};
    nix-project = import sources.nix-project;

in nix-project // { inherit pkgs; }

