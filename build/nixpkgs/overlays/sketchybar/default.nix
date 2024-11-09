self: super:

{
    sketchybar = super.sketchybar.overrideAttrs (old: {
        src = self.sources.sketchybar;
        version = "head";
        buildInputs = [ super.apple-sdk_15 ];

        # DESIGN: versionCheckHook fails to match --version to head
        nativeInstallCheckInputs = [];
    });
}
