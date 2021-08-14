home:

{
    ".ghci".source = ./ghci;
    ".psqlrc".source = ./psqlrc;
    ".cabal/config".text = import ./cabal/config.nix home;
}
