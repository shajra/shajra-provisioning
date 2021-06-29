pkgs:

{
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        duckduckgo-privacy-essentials
        simple-tab-groups
        tridactyl
    ];
}
