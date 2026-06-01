pkgs:

let
  keybindings = import ./keybindings.nix;
in
{
  package = pkgs.vscode.fhs;
  profiles.default = {
    inherit keybindings;
  };
}
