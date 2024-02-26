local icons = require("icons")
local settings = require("settings")

local cake = sbar.add("item", "cake", {
    position = "right",
    icon = {
        string = icons.cake,
        font = {size = settings.font.size - 4},
        drawing = false
    },
    label = {drawing = false},
    update_freq = 5
})

local function update()
    local command = "if " .. settings.cake_check ..
                        "; then echo true; else echo false; fi"
    sbar.exec(settings.cake_check,
              function(result) cake:set({icon = {drawing = result}}) end)
end

cake:subscribe("routine", update)
cake:subscribe("forced", update)
