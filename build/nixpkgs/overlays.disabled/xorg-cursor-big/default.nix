self: super:

super.stdenv.mkDerivation rec {

    pname = "xorg-cursor-big";
    version = "3.8";
    name = "${pname}${version}";

    src = super.fetchzip {
        url = "http://deb.debian.org/debian/pool/main/b/big-cursor/big-cursor_3.8.tar.gz";
        sha256="10pw78hm987gpi0q1m7h5vzxjxxz6fbr33p4y1dm739rpphn4wxd";
    };

    nativeBuildInputs = with self; [
        xorg.bdftopcf
        gzip
    ];

    installPhase =
        ''
        SOURCE="$src/big-cursor.bdf"

        FONT_PATH="lib/X11/fonts/misc"
        TARGET_DIR="$out/$FONT_PATH"
        FONT_FILE="cursor.pcf.gz"
        TARGET="$TARGET_DIR/$FONT_FILE"
        
        mkdir -p "$TARGET_DIR"
        bdftopcf "$SOURCE" \
            | gzip \
            > "$TARGET"
        cat > "$TARGET_DIR/fonts.dir" <<EOF
        1
        $FONT_FILE cursor
        EOF
        '';

}
