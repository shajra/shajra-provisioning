{
  stdenv,
  shajra-sources,
  clang,
  gcc,
  readline,
}:

stdenv.mkDerivation {
  pname = "sketchybar-lua";
  version = "0.0.0.0";
  src = shajra-sources.sketchybar-lua;
  nativeBuildInputs = [
    clang
    gcc
  ];
  buildInputs = [ readline ];
  installPhase = ''
    mv bin "$out"
  '';
}
