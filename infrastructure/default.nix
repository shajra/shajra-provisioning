{ checkMaterialization
, config
, sources
}:

let

    isDarwin = builtins.elem builtins.currentSystem
        np.nixpkgs-stable.lib.systems.doubles.darwin;

    hn = import ./haskell-nix {
        inherit checkMaterialization config sources isDarwin;
    };

    np = import ./nixpkgs {
        inherit config sources isDarwin;
    };

in {
    inherit hn np sources isDarwin;
}
