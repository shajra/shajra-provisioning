let
  keybindings = import ./keybindings.nix;
in
{
  profiles.default = {
    inherit keybindings;
  };
}
