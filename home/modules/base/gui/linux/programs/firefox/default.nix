config: pkgs:

let
  home = config.home.homeDirectory;
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
  extensions.common = with pkgs.nur.repos.rycee.firefox-addons; [
    auto-tab-discard
    grammarly
    tridactyl
  ];
in
{
  enable = true;
  nativeMessagingHosts = [ pkgs.tridactyl-native ];
  package = pkgs.firefox-beta;
  profiles.default = {
    id = 0;
    isDefault = true;
    extensions.packages =
      with pkgs.nur.repos.rycee.firefox-addons;
      [
        feedbroreader
      ]
      ++ extensions.common;
    inherit settings;
  };
  profiles.work = {
    id = 1;
    isDefault = false;
    extensions.packages =
      #with pkgs.nur.repos.rycee.firefox-addons;
      [
      ]
      ++ extensions.common;
    inherit settings;
  };
}
