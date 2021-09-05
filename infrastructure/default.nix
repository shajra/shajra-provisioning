{ checkMaterialization
, infraConfig
, sources
, isDevBuild
}:

let

    isDarwin = np.nixpkgs-stable.stdenv.isDarwin;

    hn = import ./haskell-nix {
        inherit checkMaterialization infraConfig sources isDarwin;
    };

    np = import ./nixpkgs {
        inherit infraConfig sources isDarwin;
    };

in {
    inherit hn np sources isDarwin isDevBuild;
}
