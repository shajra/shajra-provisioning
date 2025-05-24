{ pkgs }:

{
    colors = import ./colors.nix { inherit pkgs; };
    replaceVarsAllFiles = import ./replaceVarsAllFiles.nix { inherit pkgs; };
}
