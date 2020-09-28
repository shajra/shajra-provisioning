{ useMaterialization
, checkMaterialization
, config
, sources
}:

let

    hn = import ./haskell-nix.nix {
        inherit checkMaterialization useMaterialization config sources;
    };

    np = import ./nixpkgs.nix {
        inherit config sources;
    };

in {
    inherit hn np sources;
    isDarwin = builtins.elem builtins.currentSystem
        np.nixpkgs-stable.lib.systems.doubles.darwin;
}
