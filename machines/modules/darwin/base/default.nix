{
  build,
  config,
  pkgs,
  ...
}:

let

  hostname = config.networking.hostName;
  superUser = build.config.provision.user."${hostname}".username;

  pkgs-unstable = build.infra.np.nixpkgs.unstable;

  format = pkgs.lib.colors.format "0xff%R%G%B";
  colors = pkgs.lib.colors.transformColors format config.theme.colors;

in
{

  environment.systemPackages = [ ];
  environment.systemPath = [ "/opt/homebrew/bin" ];

  fonts.packages = build.pkgs.lists.fonts;

  homebrew = import ./homebrew;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;
  programs.zsh.enable = true;

  services.jankyborders = import services/jankyborders pkgs-unstable colors;
  services.karabiner-elements.enable = false; # DESIGN: so far, don't really need it
  services.sketchybar = import services/sketchybar config pkgs pkgs-unstable colors;
  services.skhd = import services/skhd pkgs build colors;
  services.tailscale.enable = false;

  system.checks.verifyNixPath = false;

  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.NSWindowShouldDragOnGesture = true;
  system.defaults.spaces.spans-displays = true;

  system.primaryUser = superUser;

  system.stateVersion = 6;
}
