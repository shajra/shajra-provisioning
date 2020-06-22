let

    sources = import ../sources.nix;
    pkgs =
        if builtins.currentSystem == "x86_64-darwin"
        then import sources."nixpkgs-stable-darwin" {}
        else import sources."nixpkgs-stable-linux" {};
    nix-project = import sources.nix-project;

in nix-project // { inherit pkgs; }

