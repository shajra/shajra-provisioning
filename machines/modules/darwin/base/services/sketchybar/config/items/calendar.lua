local settings = require("settings")

local cal = sbar.add("item", "calendar", {
    icon = {
        padding_right = 0,
        font = {family = settings.font.family, style = "Black"}
    },
    label = {width = 85, align = "right"},
    position = "right",
    update_freq = 15
})

local function update()
    local date = os.date("%a. %d %b.")
    local time = os.date("%I:%M %p")
    cal:set({icon = date, label = time})
end

cal:subscribe("routine", update)
cal:subscribe("forced", update)

