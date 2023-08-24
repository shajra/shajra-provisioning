self: super:

{
    lib = super.lib // {
        colors = import ./colors.nix self;
    };
}
