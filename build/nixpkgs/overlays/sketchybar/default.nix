self: super:

{
    sketchybar = super.sketchybar.overrideAttrs (old: {
        src = self.sources.sketchybar;
        version = "head";

        # DESIGN: versionCheckHook fails to match --version to head
        nativeInstallCheckInputs = [];
    });
}
