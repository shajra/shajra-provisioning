{ stdenv
, darwin
, sources
, AppKit
, Carbon
, SkyLight
}:

stdenv.mkDerivation (finalAttrs: {
    pname = "jankyborders";
    version = "0.0.0.0";

    src = sources.jankyborders;

    buildInputs = [
        AppKit
        Carbon
        SkyLight
    ];

    installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        cp ./bin/borders $out/bin/borders
        runHook postInstall
    '';

})
