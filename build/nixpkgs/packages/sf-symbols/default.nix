{
  stdenvNoCC,
  shajra-sources,
  _7zz,
}:

stdenvNoCC.mkDerivation {
  pname = "sf-symbols";
  version = "27";
  src = shajra-sources.sf-symbols;
  dontUnpack = true;
  nativeBuildInputs = [ _7zz ];
  installPhase = ''
    7zz x "$src"
    7zz x SFSymbols.pkg
    7zz x SFSymbolsPackage.pkg/Payload
    7zz x "Payload~"
    mkdir -p "$out/share/fonts/opentype" "$out/share/fonts/truetype"
    mv Library/Fonts/*.otf "$out/share/fonts/opentype"
    mv Library/Fonts/*.ttf "$out/share/fonts/truetype"
  '';
}
