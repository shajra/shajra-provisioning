config: pkgs:

let home = config.home.homeDirectory;
in

{
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        duckduckgo-privacy-essentials
        simple-tab-groups
        stylus
        tree-style-tab
        tridactyl
    ];
    profiles.default = {
        isDefault = true;
        userChrome = ''
            # DESIGN: https://github.com/piroor/treestyletab/wiki/Code-snippets-for-custom-style-rules
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
        settings = {
            "browser.download.dir" = "${home}/tmp/download";
            "browser.startup.page" = 3;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
    };
}
