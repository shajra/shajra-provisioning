config: pkgs:

let home = config.home.homeDirectory;
in

{
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        duckduckgo-privacy-essentials
        simple-tab-groups
        tree-style-tab
        tridactyl
    ];
    profiles.default = {
        isDefault = true;
        userChrome = ''
            #TabsToolbar { visibility: collapse !important; }
        '';
        settings = {
            "browser.download.dir" = "${home}/tmp/download";
            "browser.startup.page" = 3;
        };
    };
}
