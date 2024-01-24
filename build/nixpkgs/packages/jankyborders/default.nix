{ darwin }:

darwin.apple_sdk_11_0.callPackage ./jankyborders.nix {
    inherit (darwin.apple_sdk_11_0.frameworks)
        AppKit
        Carbon
        SkyLight;
}
