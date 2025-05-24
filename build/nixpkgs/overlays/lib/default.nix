final: prev:

{
    lib = prev.lib // {
        colors = prev.callPackage ./colors.nix {};
    };
}


