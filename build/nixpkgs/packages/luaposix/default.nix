{
  runCommand,
  shajra-sources,
  lib,
}:

# REVISIT: 2026-07-01: BLOCKED: Not officially supporting Lua 5.5
# See https://github.com/luaposix/luaposix/blob/master/luaposix-git-1.rockspec
#
# The last luaposix release (v36.3) works as well, but also is not yet
# officially supporting Lua 5.5.  So while bumping the limit up, it seemed fine
# to just use the latest luaposix version under development.  For the this
# latest version, we also have to remove the ldoc dependency, which is broken in
# Nixpkgs.
#
let
  rockLuaOld = "'lua >= 5.1, < 5.5'";
  rockLuaNew = "'lua >= 5.1, < 5.6'";
  rockLdocLine = " dependencies[#dependencies + 1] = 'ldoc'";
in
runCommand "luaposix-lua55" { } ''
  cp -r ${shajra-sources.luaposix} "$out"
  chmod -R u+w "$out"
  substituteInPlace "$out/luaposix-git-1.rockspec" \
    --replace-fail ${lib.escapeShellArg rockLuaOld} ${lib.escapeShellArg rockLuaNew} \
    --replace-fail ${lib.escapeShellArg rockLdocLine} ""
''
