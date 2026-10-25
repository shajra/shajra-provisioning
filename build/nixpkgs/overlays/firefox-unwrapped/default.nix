final: prev:

{
  # DESIGN: This is a hack to get smaller GUI elements without scaling all
  # applications down.  It's hard to inject these environment variables into
  # Home Manager's (re)wrapping of Firefox.
  firefox-unwrapped =
    final.runCommand "firefox"
      {
        nativeBuildInputs = [ final.makeWrapper ];
        inherit (prev.firefox-unwrapped) applicationName gtk3 meta;
      }
      ''
        for d in $(cd ${prev.firefox-unwrapped} && find . -type d)
        do
            mkdir -p $out/$d
        done
        for f in $(cd ${prev.firefox-unwrapped} && find . -type f)
        do
            ln -s ${prev.firefox-unwrapped}/$f $out/$f
        done
        rm $out/bin/firefox
        makeWrapper ${prev.firefox-unwrapped}/bin/firefox $out/bin/firefox \
            --set GDK_SCALE 1 \
            --set GDK_DPI_SCALE 0.8
      '';
}
