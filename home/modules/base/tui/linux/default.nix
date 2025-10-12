{ pkgs, ... }:

let
  pinentry = "${pkgs.pinentry-selected-linux}/bin/pinentry";
in
{
  imports = [
    ../../../ubiquity
    ../all
    # REVISIT: DEBUG: removing to understand infinite recursion
    #module-lorelei
  ];

  # REVISIT: DEBUG: removing to understand infinite recursion
  #programs.direnv-nix-lorelei.enable = true;

  programs.rbw.settings.pinentry = pkgs.pinentry-selected-linux;

  # DESIGN: Doom loads up much faster these days
  #services.emacs = import services/emacs;

  services.gpg-agent.enable = true;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.extraConfig = ''
    allow-loopback-pinentry
    pinentry-program "${pinentry}"
  '';

  xdg.mimeApps = import xdg/mimeApps;
}
