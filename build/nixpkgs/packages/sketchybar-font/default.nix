{
  stdenvNoCC,
  shajra-sources,
}:

stdenvNoCC.mkDerivation {
  pname = "sketchybar-font";
  version = "2.0.45";
  src = shajra-sources.sketchybar-font-dist;
  dontUnpack = true;
  installPhase = ''
    runHook preInstall
    install -Dm644 "$src" "$out/share/fonts/truetype/sketchybar-app-font.ttf"
    runHook postInstall
  '';
}
