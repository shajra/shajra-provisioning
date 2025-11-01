{
  dash,
  nix-project-lib,
  pinentry-all,
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
    exec "${pinentry-all}/bin/pinentry-''${PINENTRY_USER_DATA:-gtk-2}" "''${@}"
  ''
