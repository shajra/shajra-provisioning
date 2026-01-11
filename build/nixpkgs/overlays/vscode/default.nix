_final: prev:

{
  # REVISIT: 2026-01-09: Hack to circumvent Darwin security Without this hack,
  # was running into permissions problems when creating an Applications
  # directory.
  # https://github.com/nix-darwin/nix-darwin/issues/1315#issuecomment-2683833344
  vscode =
    if prev.stdenv.hostPlatform.isDarwin then
      prev.vscode.overrideAttrs (old: {
        installPhase = "whoami\n" + old.installPhase;
      })
    else
      prev.vscode;
}
