self: super:

# DESIGN: Can't update as a flake, so running this script
#
# NIX_PATH=nixpkgs=~/src/external/nixpkgs ~/src/external/nixpkgs/pkgs/applications/networking/browsers/microsoft-edge/update.py
#
let

    pinnings = {
        stable = {
            version = "116.0.1938.81";
            sha256 = "sha256-AQDhAnJWZwV2VO+RT21/0V43S1a5otlAN/wwACv57xk=";
        };
        beta = {
            version = "117.0.2045.27";
            sha256 = "sha256-1KQ5oWOxU9sUh6xdBaaoPNiahKhp5hEaXMCe5B/VZKQ=";
        };
        dev = {
            version = "118.0.2088.2";
            sha256 = "sha256-5kMStl75FpeC4L3ycOw6GyYqkJB3RUaOZlWe2gYie4g=";
        };
    };

    upstreamExpr = "${super.path}/pkgs/applications/networking/browsers"
        + "/microsoft-edge/browser.nix";

    upstreamMake = upstreamArgs:
        super.callPackage (import upstreamExpr upstreamArgs) {};

    makeEdge = channel: pinning:
        let upstreamArgs = {
                inherit channel;
                inherit (pinning) version sha256;
                revision = "1";
            };
        in {
            name = "microsoft-edge-" + channel;
            value = upstreamMake upstreamArgs;
        };

in super.lib.mapAttrs' makeEdge pinnings
