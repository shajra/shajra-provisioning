config:

let
  makeLink = config.lib.file.mkOutOfStoreSymlink;
in
{
  "fish/completions/bake.fish".source =
    makeLink /nix/var/nix/profiles/default/config/fish/bake-completion.fish;
}
