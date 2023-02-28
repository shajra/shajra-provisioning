{ mkLiteral
, theme_background
, theme_background_hl
, theme_background_tr
, theme_foreground
, theme_foreground_em
, theme_highlight
, theme_unifying
, theme_urgent
}:

let lit = mkLiteral;
    mapLit = map mkLiteral;
in

{
    "*" = {
        /* Theme settings */
        highlight = lit "bold italic";
        scrollbar = false;

        /* Theme colors */
        background =                   lit theme_background;
        background-color =             lit theme_background_tr;
        foreground =                   lit theme_foreground;
        border-color =                 lit theme_unifying;
        separatorcolor =               lit "@border-color";
        scrollbar-handle =             lit "@border-color";

        normal-background =            lit "@background";
        normal-foreground =            lit "@foreground";
        alternate-normal-background =  lit theme_background_hl;
        alternate-normal-foreground =  lit "@foreground";
        selected-normal-background =   lit theme_highlight;
        selected-normal-foreground =   lit "@foreground";

        active-background =            lit "@normal-background";
        active-foreground =            lit theme_foreground_em;
        alternate-active-background =  lit "@alternate-normal-background";
        alternate-active-foreground =  lit theme_foreground_em;
        selected-active-background =   lit "@selected-normal-background";
        selected-active-foreground =   lit theme_foreground_em;

        urgent-background =            lit "@normal-background";
        urgent-foreground =            lit theme_urgent;
        alternate-urgent-background =  lit "@alternate-normal-background";
        alternate-urgent-foreground =  lit theme_urgent;
        selected-urgent-background =   lit "@selected-normal-background";
        selected-urgent-foreground =   lit theme_urgent;
    };

    window = {
        border-radius =     lit "24px";
        background-color =  lit "@background-color";
        border =            lit "8px";
        padding =           lit "24px";
    };

    mainbox = {
        border =   0;
        padding =  0;
    };

    message = {
        border =        lit "2px 0 0";
        border-color =  lit "@separatorcolor";
        padding =       lit "1px";
    };

    textbox = {
        highlight =   lit "@highlight";
        text-color =  lit "@foreground";
    };

    listview = {
        border =        lit "2px solid 0 0";
        padding =       lit "2px 0 0";
        border-color =  lit "@separatorcolor";
        spacing =       lit "2px";
        scrollbar =     lit "@scrollbar";
    };

    element = {
        border =   0;
        padding =  lit "2px";
    };

    "element.normal.normal" = {
        background-color =  lit "@normal-background";
        text-color =        lit "@normal-foreground";
    };

    "element.normal.urgent" = {
        background-color =  lit "@urgent-background";
        text-color =        lit "@urgent-foreground";
    };

    "element.normal.active" = {
        background-color =  lit "@active-background";
        text-color =        lit "@active-foreground";
    };

    "element.selected.normal" = {
        background-color =  lit "@selected-normal-background";
        text-color =        lit "@selected-normal-foreground";
    };

    "element.selected.urgent" = {
        background-color =  lit "@selected-urgent-background";
        text-color =        lit "@selected-urgent-foreground";
    };

    "element.selected.active" = {
        background-color =  lit "@selected-active-background";
        text-color =        lit "@selected-active-foreground";
    };

    "element.alternate.normal" = {
        background-color =  lit "@alternate-normal-background";
        text-color =        lit "@alternate-normal-foreground";
    };

    "element.alternate.urgent" = {
        background-color =  lit "@alternate-urgent-background";
        text-color =        lit "@alternate-urgent-foreground";
    };

    "element.alternate.active" = {
        background-color =  lit "@alternate-active-background";
        text-color =        lit "@alternate-active-foreground";
    };

    scrollbar = {
        width =         lit "4px";
        border =        0;
        handle-color =  lit "@scrollbar-handle";
        handle-width =  lit "8px";
        padding =       0;
    };

    sidebar = {
        border =        lit "2px 0 0";
        border-color =  lit "@separatorcolor";
    };

    inputbar = {
        spacing =     0;
        text-color =  lit "@normal-foreground";
        padding =     lit "2px";
        children =    mapLit [ "prompt" "textbox-prompt-sep" "entry" "case-indicator" ];
    };

    "case-indicator, entry, prompt, button" = {
        spacing =     0;
        text-color =  lit "@normal-foreground";
    };

    "button.selected" = {
        background-color =  lit "@selected-normal-background";
        text-color =        lit "@selected-normal-foreground";
    };

    textbox-prompt-sep = {
        expand =      false;
        str =         ":";
        text-color =  lit "@normal-foreground";
        margin =      lit "0 0.3em 0 0";
    };
}
