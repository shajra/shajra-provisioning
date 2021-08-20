let

    clientColor = border: background: text: indicator: childBorder: {
        inherit border background text indicator childBorder;
    };

    base03 =    "#002b36";
    base02 =    "#073642";
    base01 =    "#586e75";  # emphasized text
    base00 =    "#657b83";  # standard text
    base0 =     "#839496";
    base1 =     "#93a1a1";  # shadowed comments
    base2 =     "#eee8d5";  # background highlights
    base3 =     "#fdf6e3";  # background
    yellow =    "#b58900";
    orange =    "#cb4b16";
    red =       "#dc322f";
    magenta =   "#d33682";
    violet =    "#6c71c4";
    blue =      "#268bd2";
    cyan =      "#2aa198";
    green =     "#859900";

in

{
    #                             border bground text   indicator cborder
    urgent          = clientColor base1  base2   orange red       base1;
    focused         = clientColor green  green   base3  yellow    green;
    focusedInactive = clientColor base1  base2   violet base00    base1;
    unfocused       = clientColor base1  base2   base00 base00    base1;
    placeholder     = clientColor base03 base02  base0  base03    base03;
    background      = base0;
}
