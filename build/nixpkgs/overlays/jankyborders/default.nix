final: prev:

{
  jankyborders = prev.jankyborders.overrideAttrs (_old: {
    src = final.sources.jankyborders;
    version = "head";
    buildInputs = [ prev.apple-sdk_15 ];
  });
}
