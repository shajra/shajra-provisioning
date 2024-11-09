self: super:

{
    sketchybar = super.sketchybar.overrideAttrs (old: {
        src = self.sources.sketchybar;
        version = "head";
    });
}
