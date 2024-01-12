local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
    blur_radius = 20,
    border_color = colors.unifying,
    border_width = 4,
    color = colors.primary.background,
    corner_radius = 9,
    height = 36,
    margin = 4,
    padding_left = 10,
    padding_right = 10,
    shadow = false,
    sticky = true,
    topmost = "window",
    y_offset = 4
})
