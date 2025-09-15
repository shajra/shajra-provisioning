{
  nix-project-lib,
  coreutils,
  dash,
  i3,
}:

let
  progName = "i3-dpi";
  meta.description = "Change DPI for I3 window manager";
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;

    runtimeShell = "${dash}/bin/dash";
    envKeep = [
      "DISPLAY"
      "HOME"
    ];
    pathKeep = [
      "xrandr"
      "xrdb"
    ];
    pathPackages = [
      coreutils
      i3
    ];
  }
  ''
    set -eu


    DPI="''${1:-235}"
    XRESOURCES=~/.Xresources.dpi


    main()
    {
        configure_dpi
        restart_i3
    }

    configure_dpi()
    {
        { echo "*dpi: $DPI"; echo "Xft.dpi: $DPI"; } > "$XRESOURCES"
        xrdb -merge "$XRESOURCES"
        xrandr --dpi "$DPI"
    }

    restart_i3()
    {
        i3-msg restart
    }


    main
  ''
