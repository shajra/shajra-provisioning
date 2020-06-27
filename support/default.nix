let

    sources = import ../sources-unified.nix;
    pkgs = import sources.nixpkgs-stable { config = {}; };
    nix-project = import sources.nix-project;

in nix-project // { inherit pkgs; }

