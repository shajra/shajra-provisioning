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
            # DESIGN: Don't feel these are useful any more
            #duckduckgo-privacy-essentials
            #simple-tab-groups
            #stylus
            #tree-style-tab
            tridactyl
        ];
        /*
        # DESIGN: https://github.com/piroor/treestyletab/wiki/Code-snippets-for-custom-style-rules
        userChrome = ''
            #main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar > .toolbar-items {
              opacity: 0;
              pointer-events: none;
            }
            #main-window:not([tabsintitlebar="true"]) #TabsToolbar {
                visibility: collapse !important;
            }
            #main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar .titlebar-spacer {
                border-inline-end: none;
            }
            #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
                display: none;
            }
        '';
        */
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
            # DESIGN: https://github.com/piroor/treestyletab/wiki/Code-snippets-for-custom-style-rules
            #"toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
    };
}
