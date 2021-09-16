let

    clientColor = border: background: text: indicator: childBorder: {
        inherit border background text indicator childBorder;
    };

    base03     = "#002b36";
    base02     = "#073642";
    base015    = "#2f525b";
    base01     = "#586e75";  # emphasized text
    base00     = "#657b83";  # standard text
    base0      = "#839496";
    base1      = "#93a1a1";  # shadowed comments
    base15     = "#c0c4bb";
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

    focus_bg      = green;
    focus_fg      = base3;
    unfocus_bg    = base15;
    unfocus_fg_em = base3;
    unfocus_fg    = base015;

in

{
    # DESIGN: The "child border" is the prominent border.  The "border" is just
    # the very thin lines around the title.
    #
    #                             border   bground    text          indicator cborder
    urgent          = clientColor base01   unfocus_bg magenta       violet    unfocus_bg;
    focused         = clientColor focus_bg focus_bg   focus_fg      base00    focus_bg;
    focusedInactive = clientColor base01   unfocus_bg unfocus_fg_em base1     unfocus_bg;
    unfocused       = clientColor base01   unfocus_bg unfocus_fg    base1     unfocus_bg;
    placeholder     = clientColor base3    base3      base01        base01    base3;
}
