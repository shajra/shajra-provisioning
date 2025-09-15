{ pkgs, ... }:

let

  # DESIGN: Gives freedom to chose the pin entry program explicitly, rather
  # than rely on built-in fallbacks.  Gnome3's curses fallback is not
  # convenient, but using Gnome3 as a default for Solarized theming.
  pinentry = pkgs.writers.writeDash "pinentry" ''
    case "$PINENTRY_USER_DATA" in
        curses) PINENTRY="${pkgs.pinentry.curses}" ;;
        emacs)  PINENTRY="${pkgs.pinentry.emacs}"  ;;
        gnome3) PINENTRY="${pkgs.pinentry.gnome3}" ;;
        gtk2)   PINENTRY="${pkgs.pinentry.gtk2}"   ;;
        qt)     PINENTRY="${pkgs.pinentry.qt}"     ;;
        tty)    PINENTRY="${pkgs.pinentry.tty}"    ;;
        *)      PINENTRY="${pkgs.pinentry.gtk2}"   ;;
    esac
    exec "$PINENTRY/bin/pinentry" "''${@}"
  '';

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
