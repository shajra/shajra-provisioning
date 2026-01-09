local settings = require("settings")
local colors = require("colors")

-- Equivalent to the --default domain
sbar.default({
    updates = "when_shown",
    icon = {
        font = {family = "SF Pro", style = "Bold", size = settings.font.size},
        color = colors.primary.foreground,
        padding_left = settings.paddings,
        padding_right = settings.paddings
    },
    label = {
        font = {
            family = settings.font.family,
            style = "Semibold",
            size = settings.font.size
        },
        color = colors.primary.foreground,
        padding_left = settings.paddings,
        padding_right = settings.paddings
    },
    graph = {
        color = colors.primary.foreground,
        fill_color = colors.primary.foreground
    },
    background = {height = 26, border_width = 4},
    popup = {
        background = {
            border_width = 2,
            corner_radius = 9,
            border_color = colors.unifying,
            color = colors.secondary.background,
            shadow = {drawing = true}
        },
        blur_radius = 20
    },
    padding_left = 5,
    padding_right = 5
})

