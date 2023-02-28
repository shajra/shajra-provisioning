self: super:

super.runCommand "xorg-cursor-big" {
    nativeBuildInputs = with self; [
        bdfresize
        xorg.bdftopcf
        gzip
        pcf2bdf
    ];
}
''
FONT_PATH="lib/X11/fonts/misc"
FONT_FILE="cursor.pcf.gz"
SOURCE="${super.xorg.fontcursormisc}/$FONT_PATH/$FONT_FILE"
TARGET_DIR="$out/$FONT_PATH"
TARGET="$TARGET_DIR/$FONT_FILE"

mkdir -p "$TARGET_DIR"
pcf2bdf "$SOURCE" \
    | bdfresize -f 3 \
    | bdftopcf \
    | gzip \
    > "$TARGET"
cat > "$TARGET_DIR/fonts.dir" <<EOF
1
$FONT_FILE cursor
EOF
''
