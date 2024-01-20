local icons = require("icons")
local colors = require("colors")

local yabai = sbar.add("item", {
    icon = {color = colors.info, font = {style = "Bold"}, width = 0},
    label = {color = colors.info, width = 0}
})

yabai:subscribe("window_focus", function(env)
    local icon = icons.yabai[env.STATE] or ""
    local label = ""
    if env.STATE == "stack" then
        label = "[" .. env.STACK_INDEX .. "/" .. env.STACK_LAST .. "]"
    end
    local icon_width, label_width = 0, 0
    if icon ~= "" then icon_width = 30 end
    if label ~= "" then label_width = 40 end
    sbar.animate("sin", 10, function()
        yabai:set({
            icon = {string = icon, width = icon_width},
            label = {string = label, width = label_width}
        })
    end)
end)
