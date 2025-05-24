config: pkgs:

{
    "fish/completions/notify-time.fish".source = fish/completions/notify-time.fish;
    "tridactyl/tridactylrc".source = pkgs.replaceVars tridactyl/tridactylrc {
        theme_url  = config.theme.external.tridactyl.url;
        theme_name = config.theme.external.tridactyl.name;
    };
}
