self: super:
let script = super.callPackage (import ./script.nix) {};
in super.writeShellScriptBin "lan-jelly" script
