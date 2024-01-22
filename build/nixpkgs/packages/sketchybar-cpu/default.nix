{ clang
, lib
, stdenv}:

stdenv.mkDerivation {
    pname = "sketchybar-cpu";
    version = "0.0.0.0";
    src = lib.sourceFilesBySuffices ./. [ "makefile" ".c" ".h" ];
    nativeBuildInputs = [ clang ];
    installPhase = ''
        mkdir -p "$out/bin"
        mv helper "$out/bin/sketchybar-cpu"
    '';
}
