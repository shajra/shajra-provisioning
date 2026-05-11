{ build, config, ... }:

{
  imports = [ ../../ubiquity ];
  home.extraPackages = build.pkgs.lists.programming.go;

  programs.go = {
    enable = true;
    package = null;
    env = {
      GOPATH = "${config.xdg.dataHome}/go";
      GOMODCACHE = "${config.xdg.cacheHome}/go/pkg/mod";
      GOCACHE = "${config.xdg.cacheHome}/go-build";
      GOBIN = "${config.home.homeDirectory}/.local/bin";
    };
  };
}
