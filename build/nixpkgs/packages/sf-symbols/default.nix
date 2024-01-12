{ stdenvNoCC
, sources
, p7zip
}:

stdenvNoCC.mkDerivation {
    pname = "sf-symbols";
    version = "5";
    src = sources.sf-symbols;
    dontUnpack = true;
    nativeBuildInputs = [ p7zip ];
    installPhase = ''
        7z x "$src"
        cd SFSymbols
        7z x "SF Symbols.pkg"
        7z x "Payload~"
        mkdir -p "$out/share/fonts/opentype" "$out/share/fonts/truetype"
        mv Library/Fonts/*.otf "$out/share/fonts/opentype"
        mv Library/Fonts/*.ttf "$out/share/fonts/truetype"
    '';
}
