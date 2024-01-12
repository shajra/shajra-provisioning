{ stdenvNoCC
, sources
}:

stdenvNoCC.mkDerivation {
    pname = "sketchybar-font";
    version = "1.0.21";
    src = sources.sketchybar-font-dist;
    dontUnpack = true;
    installPhase = ''
        runHook preInstall
        install -Dm644 "$src" "$out/share/fonts/truetype/sketchybar-app-font.ttf"
        runHook postInstall
    '';
}
