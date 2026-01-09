local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
    border_color = colors.unifying,
    border_width = 4,
    color = colors.primary.background,
    corner_radius = 0,
    height = 43,
    margin = -4,
    -- DESIGN: The padding ends up looking more symmetric with the date/time
    -- text on the right.
    padding_left = 14,
    padding_right = 10,
    shadow = false,
    sticky = true,
    topmost = "window",
    y_offset = -4
})
