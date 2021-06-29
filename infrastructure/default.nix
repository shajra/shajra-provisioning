{ checkMaterialization
, infraConfig
, sources
}:

let

    isDarwin = builtins.elem builtins.currentSystem
        np.nixpkgs-stable.lib.systems.doubles.darwin;

    hn = import ./haskell-nix {
        inherit checkMaterialization infraConfig sources isDarwin;
    };

    np = import ./nixpkgs {
        inherit infraConfig sources isDarwin;
    };

in {
    inherit hn np sources isDarwin;
}
