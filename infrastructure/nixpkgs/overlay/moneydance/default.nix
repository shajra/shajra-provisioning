# DESIGN: autoPatchelfHook was insufficient, so manually specifying all rpaths
# mimicing heavily the adoptopenjdk Nix expression in Nixpkgs.

self: super:

let 

    rpathInternal = super.lib.concatStringsSep ":" [
        "$out/jre/lib/jli"
        "$out/jre/lib/server"
        "$out/jre/lib"
    ];

    rpathExternal = with super; lib.strings.makeLibraryPath [
        alsaLib 
        atk 
        cairo
        elfutils
        ffmpeg 
        fontconfig 
        freetype 
        gdk_pixbuf
        glib 
        gtk3 
        libGL
        libxml2 
        libxslt 
        pango 
        stdenv.cc.cc
        stdenv.cc.libc 
        xorg.libX11 
        xorg.libXext 
        xorg.libXi 
        xorg.libXp 
        xorg.libXrender
        xorg.libXt
        xorg.libXtst 
        xorg.libXxf86vm 
        zlib 
    ];

in

super.stdenv.mkDerivation rec {

    pname = "moneydance";
    #version = "2019.1_1855";  # stable
    #version = "2019.3_1880";  # stable
    #version = "2020.0_1919";  # stable
    #version = "2020.2_1929";  # stable
    version = "2021.1_3069";  # stable

    #version = "2021.01.18";    # preview
    #version = "2021.02.24";    # preview

    name = "${pname}-${version}";

    src = super.fetchzip {
        url = "https://infinitekind.com/stabledl/${version}/moneydance-linux.tar.gz";
        #url = "https://infinitekind.com/previewdl/current/moneydance-linux.tar.gz";

        # stable
        #sha256 = "1dhwjay7j9q8ss58slzp1yxyxqn0bm56a2dwmnaz8566a7cpawva";
        #sha256 = "17z7z74c7v9d6gjp6mrdsaqnxva461kcck310xrs2kpzlzlj6l0m";
        #sha256 = "0g1qxjwxr8nzczngfa2igjwmcbsikm7kh0vigz7skhrki5vx38h5";
        #sha256 = "1dyi81lwiqlq636lb48im0r7awcgxsyjmmqc8sb7n3isp7ccpnjz";
        #sha256 = "12nigji1i9n0sgkq6slazksy6br28bnksrzyrk1aiyzqqj8jy9vv";
        sha256 = "04cclqbnr2a34nxwx84mv7b6mxdp862gxgf2rspf4pxcdyl3v9w7";

        # preview
        #sha256 = "09l164a6hg5krib7v6cf4w81hhw17nara5xknsfzyv110ip6m79q";
        #sha256 = "1xgpgy4v3akajdz294jp8qcv2v41x57w522fw03f3lh811cw39na";
    };

    nativeBuildInputs = with super; [ 
        gnused 
        makeWrapper 
        wrapGAppsHook 
        glib 
        gnome3.adwaita-icon-theme
        #hicolor-icon-theme 
    ];

    dontStrip = 1;

    installPhase = ''
        mkdir "$out"
        for f in bin lib jre version.txt
        do cp -r "$src/$f" "$out"
        done
        chmod -R +rwX "$out"
        sed -i \
            -e 's|=`pwd`|='"$out/bin"'|' \
            "$out/bin/moneydance"
        rm \
            "$out/jre/lib/libavplugin-ffmpeg-56.so" \
            "$out/jre/lib/libavplugin-54.so" \
            "$out/jre/lib/libavplugin-56.so" \
            "$out/jre/lib/libglassgtk2.so"
        #wrapProgram "$out/bin/moneydance" \
        #    --set VM_OPTIONS \
        #        -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel
        '';

    postFixup = ''
        rpath="${rpathExternal}:${rpathInternal}"
        find $out -type f -perm -0100 \
            -exec patchelf \
            --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
            --set-rpath "$rpath" {} \;
        find $out -name "*.so" -exec patchelf --set-rpath "$rpath" {} \;
    '';

}
