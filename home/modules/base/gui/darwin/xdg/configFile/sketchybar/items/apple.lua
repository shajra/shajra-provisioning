local icons = require("icons")

local popup_toggle = "sketchybar --set $NAME popup.drawing=toggle"

local apple_logo = sbar.add("item", "apple", {
    padding_right = 15,
    click_script = popup_toggle,
    icon = {string = icons.apple, font = {family = "SF Pro", style = "Black"}},
    label = {drawing = false},
    popup = {height = 35}
})

local apple_prefs = sbar.add("item", "apple.prefs", {
    position = "popup." .. apple_logo.name,
    icon = icons.preferences,
    label = "Preferences"
})

apple_prefs:subscribe("mouse.clicked", function(_)
    os.execute("open -a 'System Preferences'")
    apple_logo:set({popup = {drawing = false}})
end)
