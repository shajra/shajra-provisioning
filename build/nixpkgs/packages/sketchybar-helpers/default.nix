{
  clang,
  shajra-sources,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "sketchybar-helpers";
  version = "0.0.0.0";
  src = "${shajra-sources.dotfiles-felixkratz}/.config/sketchybar/helpers";
  nativeBuildInputs = [ clang ];
  installPhase = ''
    mkdir -p "$out/bin"
    mv event_providers/cpu_load/bin/cpu_load "$out/bin/sketchybar-cpu"
    mv event_providers/network_load/bin/network_load "$out/bin/sketchybar-network"
    mv menus/bin/menus "$out/bin/sketchybar-menus"
  '';
}
