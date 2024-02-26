{ stdenv
, sources
, clang
, gcc
, readline
}:

stdenv.mkDerivation {
    pname = "sketchybar-lua";
    version = "0.0.0.0";
    src = sources.sketchybar-lua;
    patches = [ ./patch ];
    nativeBuildInputs = [ clang gcc ];
    buildInputs = [ readline ];
    installPhase = ''
        mv bin "$out"
    '';
}
