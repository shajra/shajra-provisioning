self: super:
let script = super.callPackage (import ./script.nix) {};
in super.writeShellScriptBin "i3-dpi" script
