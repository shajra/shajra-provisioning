self: super:

super.starship.overrideAttrs (oldAttrs: {
    #features = ["notify-rust"];
})
