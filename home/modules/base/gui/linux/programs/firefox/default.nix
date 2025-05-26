config: pkgs:

let
    home = config.home.homeDirectory;
in

{
    enable = true;
    nativeMessagingHosts = [ pkgs.tridactyl-native ];
    package = pkgs.firefox-beta;
    profiles.default = {
        isDefault = true;
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
            auto-tab-discard
            feedbroreader
            grammarly
            tridactyl
        ];
        settings = {
            "browser.download.dir" = "${home}/tmp/download";
            "browser.startup.page" = 3;
            "services.sync.engine.addons" = true;
            "services.sync.engine.addresses" = false;
            "services.sync.engine.bookmarks" = true;
            "services.sync.engine.creditcards" = false;
            "services.sync.engine.history" = true;
            "services.sync.engine.passwords" = false;
            "services.sync.engine.prefs" = true;
            "services.sync.engine.tabs" = true;
            "sidebar.revamp" = true;
            "sidebar.verticalTabs" = true;
        };
    };
}
