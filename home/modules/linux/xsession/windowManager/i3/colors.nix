let clientColor = border: background: text: indicator: childBorder: {
        inherit border background text indicator childBorder;
    };
in

{
    urgent =
        clientColor "#dc322f" "#eee8d5" "#dc322f" "#dc322f" "#dc322f";
    focused =
        clientColor "#859900" "#859900" "#fdf6e3" "#859900" "#859900";
    focusedInactive =
        clientColor "#073642" "#eee8d5" "#6c71c4" "#586e75" "#073642";
    unfocused =
        clientColor "#073642" "#eee8d5" "#657b83" "#586e75" "#073642";

    #background = ;
}
