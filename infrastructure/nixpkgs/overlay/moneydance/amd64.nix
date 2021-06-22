self: super:

super.stdenv.mkDerivation rec {

    pname = "moneydance";
    version = "2019.1_1855";
    #version = "2017.10_1706";
    #version = "2017.8_1691";
    name = "${pname}-${version}";

    src = super.fetchzip {
        url = "https://infinitekind.com/stabledl/${version}/Moneydance_linux_amd64.tar.gz";
        sha256 = "15w3liqx8dj7yv24xvfr0pa08slacnl59gb11s8zzrq2j8zjvb8s";
        #sha256 = "1i127vh4rp6vsf1nalpn5xyniyz6ch0skvihrp82kjv4dmsggp9d";
        #sha256 = "000yjmyayfc7pv3pyd8xhd4x4z5n7rjp1s586xpw5j1m56swjr0m";
    };

    nativeBuildInputs = with super; [ autoPatchelfHook gnused makeWrapper ];

    buildInputs = with super; [ ffmpeg glib gtk3 xorg.libXtst pango zlib ];

    # TODO: I'd like to upgrade to 2019.1 above, but it requires not only Java
    # 11, but also the JavaFX module, which has been factored out.  And the
    # "AdoptOpenJdk" builds that Nix relies on don't package JavaFX.  The
    # Oracle JRE comes with JavaFX but in Nixpkgs, this build is old (Java 8).
    # Updating the Oracle Nix expression from Java 8 to 11 is probably work
    # since given some of the major changes that have happened between those
    # two versions.
    # 
    installPhase =
        ''
        runHook preInstall

        mkdir -p "$out/bin"
        for f in .install4j lib jre Moneydance Moneydance.vmoptions resources
        #for f in .install4j jars Moneydance Moneydance.vmoptions resources
        do cp -r "$src/$f" "$out"
        done
        sed -i \
            -e 's|=`pwd`|='"$out"'|' \
            "$out/Moneydance"
        #wrapProgram "$out/Moneydance" \
        #    --set INSTALL4J_JAVA_HOME "${super.adoptopenjdk-jre-bin}"
        #wrapProgram "$out/Moneydance" \
        #    --set INSTALL4J_JAVA_HOME "${super.jre}"
        chmod -R +rwX "$out/jre"
        rm \
            "$out/jre/lib/libavplugin-ffmpeg-56.so" \
            "$out/jre/lib/libavplugin-54.so" \
            "$out/jre/lib/libavplugin-56.so" \
            "$out/jre/lib/libglassgtk2.so"
        ln -s ../Moneydance "$out/bin/moneydance"

        runHook postInstall
        '';

}
