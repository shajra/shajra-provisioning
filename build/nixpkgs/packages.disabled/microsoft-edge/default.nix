{ path
, callPackage
, microsoft-edge
}:

# DESIGN: Hard to update as a flake, so running this script
#
# NIX_PATH=nixpkgs=/home/tnks/src/external/nixpkgs \
#     ~/src/external/nixpkgs/pkgs/applications/networking/browsers/microsoft-edge/update.sh
#
let makeEdge = "${path}/pkgs/applications/networking/browsers/microsoft-edge/browser.nix";
    metadata = {
        channel = "stable";
        version = "110.0.1587.50";
        revision = "1";
        sha256 = "sha256:0c6xvmszn4bqr509pfas3npkmzbhad6nqlyhymy44af566c02i91";
    };
in callPackage (import makeEdge metadata) {}
