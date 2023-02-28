self: super:

super.lib.mapAttrs
    (p: t: self.callPackage (./. + "/${p}") {})
    (builtins.readDir ./.)
