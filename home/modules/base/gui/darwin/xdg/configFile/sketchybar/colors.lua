return {
    transparent = function (color)
        return (color & 0x00FFFFFF) | 0x8F000000
    end,

    blue = tonumber("@colors_blue@"),
    red = tonumber("@colors_red@"),

    unifying = tonumber("@colors_unifying@"),
    info = tonumber("@colors_info@"),
    warning = tonumber("@colors_warning@"),
    urgent = tonumber("@colors_urgent@"),

    primary = {
        background = tonumber("@colors_primary_bg@"),
        foreground = tonumber("@colors_primary_fg@")
    },
    secondary = {
        background = tonumber("@colors_secondary_bg@"),
        foreground = tonumber("@colors_secondary_fg@")
    },
    selected = {
        background = tonumber("@colors_selected_bg@"),
        foreground = tonumber("@colors_selected_fg@")
    },
    unselected = {
        background = tonumber("@colors_unselected_bg@"),
        foreground = tonumber("@colors_unselected_fg@")
    }
}
