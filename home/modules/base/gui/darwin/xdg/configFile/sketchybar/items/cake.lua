local icons = require("icons")
local settings = require("settings")

local cake = sbar.add("item", "cake", {
    position = "right",
    icon = {string = icons.cake, drawing = false},
    label = {drawing = false},
    update_freq = 5
})

local function update()
    local drawing = false
    if os.execute(settings.cake_check) then drawing = true end
    cake:set({icon = {drawing = drawing}})
end

cake:subscribe("routine", update)
cake:subscribe("forced", update)
