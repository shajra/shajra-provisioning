{ coreutils
, i3
, xrandr 
, xrdb
}:

''
#!/bin/sh -eu


DPI="''${1:-235}"
XDEFAULTS="$HOME/.Xdefaults.dpi"


${coreutils}/bin/echo "*dpi: $DPI" > "$XDEFAULTS"
${xrdb}/bin/xrdb -merge "$XDEFAULTS"
${xrandr}/bin/xrandr --dpi "$DPI"
${i3}/bin/i3-msg restart
''
