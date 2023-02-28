self: super:

super.stdenv.mkDerivation rec {

    pname = "moneydance";
    version = "2019.1_1855";
    name = "${pname}-${version}";

    src = super.fetchzip {
        url = "https://infinitekind.com/stabledl/${version}/moneydance-linux.tar.gz";
        sha256 = "1dhwjay7j9q8ss58slzp1yxyxqn0bm56a2dwmnaz8566a7cpawva";
    };

    nativeBuildInputs = with super; [ autoPatchelfHook gnused makeWrapper ];

    buildInputs = with super; [ ffmpeg glib gtk3 xorg.libXtst pango zlib ];

    runtimeDependencies = with super; [ fontconfig ];

    installPhase =
        ''
        mkdir "$out"
        for f in bin lib jre version.txt
        do cp -r "$src/$f" "$out"
        done
        chmod -R +rwX "$out"
        sed -i \
            -e 's|=`pwd`|='"$out/bin"'|' \
            "$out/bin/moneydance"
        #{
        #    echo version=1 
        #    echo sequence.allfonts=default
        #} >> "$out/jre/lib/fontconfig.properties"
        rm \
            "$out/jre/lib/libavplugin-ffmpeg-56.so" \
            "$out/jre/lib/libavplugin-54.so" \
            "$out/jre/lib/libavplugin-56.so" \
            "$out/jre/lib/libglassgtk2.so"
        '';

}
