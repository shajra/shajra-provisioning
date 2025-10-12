{
  dash,
  nix-project-lib,
  pinentry,
}:

let

  progName = "pinentry";

  # DESIGN: Gives freedom to chose the pin entry program explicitly, rather than
  # rely on built-in fallbacks.  Gnome3's curses fallback is not convenient, but
  # using Gnome3 as a default for Solarized theming.
  meta.description = "User-controlled pinentry";

in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    runtimeShell = "${dash}/bin/dash";
    envCleaned = false;
  }
  ''
    case "$PINENTRY_USER_DATA" in
        curses) PINENTRY="${pinentry.curses}" ;;
        emacs)  PINENTRY="${pinentry.emacs}"  ;;
        gnome3) PINENTRY="${pinentry.gnome3}" ;;
        gtk2)   PINENTRY="${pinentry.gtk2}"   ;;
        qt)     PINENTRY="${pinentry.qt}"     ;;
        tty)    PINENTRY="${pinentry.tty}"    ;;
        *)      PINENTRY="${pinentry.gtk2}"   ;;
    esac
    exec "$PINENTRY/bin/pinentry" "''${@}"
  ''
