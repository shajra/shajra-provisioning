self: super:

{
    jankyborders = super.jankyborders.overrideAttrs (old: {
        src = self.sources.jankyborders;
        version = "head";
    });
}
