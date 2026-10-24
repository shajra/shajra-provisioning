{
  lua5_5,
  shajra-sources,
}:

lua5_5.pkgs.buildLuarocksPackage {
  pname = "luaposix";
  version = "dev-1";
  src = shajra-sources.luaposix;

  postPatch = ''
    substituteInPlace luaposix-dev-1.rockspec \
      --replace-fail "build_dependencies = {'gvvaughan/ldoc'}" \
      "build.build_command = build.build_command .. ' LDOC=true'"
  '';
}
