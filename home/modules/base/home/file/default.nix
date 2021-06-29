home:

{
    ".ghci".source = ./ghci;
    ".inputrc".source = ./inputrc;
    ".psqlrc".source = ./psqlrc;
    ".cabal/config".text = import ./cabal/config.nix home;
}
