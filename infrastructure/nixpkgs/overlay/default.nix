self: super:

let

    overlays =
        super.lib.mapAttrs
            (p: t: import (./. + "/${p}") self super)
            (builtins.readDir ./.);

    xorg = super.xorg // {
    };

in overlays  # // { inherit xorg; }
