# DESIGN: There used to be a headache of Moneydance relying on JavaFX and JREs
# in Nixpkgs being out of date.  In that time, I patched the JRE provided by the
# Moneydance distribution.  These are the other files in this directory (left
# there in case I need to revert to that technique later).  But now that Nixpkgs
# is on a current-enough JRE, just using that here.

self: super:

let 

    moneydance-lib = super.stdenv.mkDerivation rec {
        pname = "moneydance-lib";
        version = "2021.1_3069";
        name = "${pname}-${version}";
        src = super.moneydance-dist;
        phases = [ "installPhase" ];
        installPhase = ''
            cp -r "$src/lib" "$out"
        '';
    };

in super.writers.writeDashBin "moneydance" ''
    for jarfile in ${moneydance-lib}/*.jar
    do CLASSPATH="$CLASSPATH:$jarfile"
    done

    "${super.jre}/bin/java" \
        -classpath "$CLASSPATH" \
        -Dawt.useSystemAAFontSettings=on \
        -Dswing.aatext=true \
        -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel \
        -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel \
        com.moneydance.apps.md.controller.Main

''
