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
    isDarwin = builtins.elem builtins.currentSystem
        np.nixpkgs-stable.lib.systems.doubles.darwin;
    nixpkgs-stable = np.nixpkgs-stable;
    nixpkgs-unstable = np.nixpkgs-unstable;
    haskell-nix = hn.haskell-nix;
    hs.np.fromPackages = np.hs.fromPackages;
    hs.np.fromNixpkgs = np.hs.fromNixpkgs;
    hs.hn = hn;
}
