{
  stdenvNoCC,
  adw-gtk3,
  shajra-sources,
}:

stdenvNoCC.mkDerivation {
  pname = "adw-gtk3-solarized";
  inherit (adw-gtk3) version;

  src = shajra-sources.adw-colors;

  dontUnpack = true;

  installPhase = ''
    runHook preInstall

    themeDir="$out/share/themes/adw-gtk3-solarized"
    mkdir -p "$(dirname "$themeDir")"
    cp -R "${adw-gtk3}/share/themes/adw-gtk3" "$themeDir"
    chmod -R u+w "$themeDir"

    substituteInPlace "$themeDir/index.theme" \
      --replace-fail "adw-gtk3" "adw-gtk3-solarized"

    cat "$src/themes/adw-solarized/gtk3-light.css" \
      >> "$themeDir/gtk-3.0/gtk.css"
    cat "$src/themes/adw-solarized/gtk4-light.css" \
      >> "$themeDir/gtk-4.0/gtk.css"

    runHook postInstall
  '';
}
