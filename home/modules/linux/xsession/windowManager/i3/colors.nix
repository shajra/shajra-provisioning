let

    clientColor = border: background: text: indicator: childBorder: {
        inherit border background text indicator childBorder;
    };

    base03     = "#002b36";
    base02     = "#073642";
    base01     = "#586e75";  # emphasized text
    base00     = "#657b83";  # standard text
    base0      = "#839496";
    base1      = "#93a1a1";  # shadowed comments
    base2      = "#eee8d5";  # background highlights
    base3      = "#fdf6e3";  # background
    yellow     = "#b58900";
    orange     = "#cb4b16";
    red        = "#dc322f";
    magenta    = "#d33682";
    violet     = "#6c71c4";
    blue       = "#268bd2";
    cyan       = "#2aa198";
    green      = "#859900";

in

{
    # DESIGN: The "child border" is the prominent border.  The "border" is just
    # the very thin lines around the title.
    #
    #                             border bground text   indicator cborder
    urgent          = clientColor base01 base1   orange red       base1;
    focused         = clientColor green  green   base3  cyan      green;
    focusedInactive = clientColor base01 base1   base3  violet    base1;
    unfocused       = clientColor base01 base1   base02 violet    base1;
    placeholder     = clientColor base3  base3   base01 base01    base3;
}
