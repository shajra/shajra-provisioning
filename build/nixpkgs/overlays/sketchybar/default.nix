final: prev:

{
  sketchybar = prev.sketchybar.overrideAttrs (_old: {
    src = final.shajra-sources.sketchybar;
    version = "head";
    buildInputs = [ prev.apple-sdk_15 ];

    # DESIGN: versionCheckHook fails to match --version to head
    nativeInstallCheckInputs = [ ];
  });
}
