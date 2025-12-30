{
  callPackage,
  symlinkJoin,
  runCommand,
  pkgs,
}:
{ src, files }:
substitutions:

let
  replaceVarsWithQuiet = callPackage (runCommand "replace-vars-with-quiet" { } ''
    cp "${pkgs.path}/pkgs/build-support/replace-vars/replace-vars-with.nix" "$out"
    substituteInPlace "$out" --replace "--replace-fail" "--replace-quiet"
  '') { };
  replaceVarsQuiet = src: replacements: (replaceVarsWithQuiet { inherit src replacements; });
  substitutedFiles = map (
    file:
    runCommand "replacedVarsFile-${baseNameOf file}" { } ''
      mkdir -p $out/$(dirname "${file}")
      ln -s "${replaceVarsQuiet "${src}/${file}" substitutions}" "$out/${file}"
    ''
  ) files;
in
symlinkJoin {
  name = "replacedVarsAllFiles";
  paths = substitutedFiles;
}
