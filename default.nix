let config = import ./config.nix;
    sources = import ./sources;
in

args@{ buildSet ? config.buildSet
, buildInfrastructure ? config.buildInfrastructure
, checkMaterialization ? config.haskell-nix.checkMaterialization
}:

(import ./build.nix args).pkgs
