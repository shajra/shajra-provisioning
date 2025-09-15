config: pkgs:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  "fish/completions/notify-time.fish".source = fish/completions/notify-time.fish;
  "tridactyl/tridactylrc".source = pkgs.replaceVars tridactyl/tridactylrc {
    bind_bmarks = if isDarwin then "<C-b>" else "<AC-b>";
    theme_name = config.theme.external.tridactyl.name;
    theme_url = config.theme.external.tridactyl.url;
    neovide = "${config.programs.neovide.package}/bin/neovide";
  };
}
