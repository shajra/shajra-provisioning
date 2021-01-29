{ useMaterialization
, checkMaterialization
, config
, sources
}:

let

    isDarwin = builtins.elem builtins.currentSystem
        np.nixpkgs-stable.lib.systems.doubles.darwin;

    hn = import ./haskell-nix.nix {
        inherit checkMaterialization useMaterialization config sources isDarwin;
    };

    np = import ./nixpkgs.nix {
        inherit config sources isDarwin;
    };

in {
    inherit hn np sources isDarwin;
}
