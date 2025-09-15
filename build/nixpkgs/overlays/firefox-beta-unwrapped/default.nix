final: prev:

{
  # DESIGN: This is a hack to get smaller GUI elements without scaling all
  # applications down.  It's hard to inject these environment variables into
  # Home Manager's (re)wrapping of Firefox.
  firefox-beta-unwrapped =
    final.runCommand "firefox-beta"
      {
        nativeBuildInputs = [ final.makeWrapper ];
        inherit (prev.firefox-beta-unwrapped) applicationName gtk3 meta;
      }
      ''
        for d in $(cd ${prev.firefox-beta-unwrapped} && find . -type d)
        do
            mkdir -p $out/$d
        done
        for f in $(cd ${prev.firefox-beta-unwrapped} && find . -type f)
        do
            ln -s ${prev.firefox-beta-unwrapped}/$f $out/$f
        done
        rm $out/bin/firefox-beta
        makeWrapper ${prev.firefox-beta-unwrapped}/bin/firefox-beta $out/bin/firefox-beta \
            --set GDK_SCALE 1 \
            --set GDK_DPI_SCALE 0.8
      '';
}
