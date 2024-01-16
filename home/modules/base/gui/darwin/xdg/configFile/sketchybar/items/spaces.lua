local colors = require("colors")
local emojis = require("emojis")
local settings = require("settings")
local spaces = {}

local function mouse_click(env)
    if env.BUTTON == "right" then
        os.execute("yabai -m space --destroy " .. env.SID ..
                       " && sketchybar --trigger space_change")
    else
        os.execute("yabai -m space --focus " .. env.SID)
    end
end

local function space_selection(env)
    local bg_color = env.SELECTED == "true" and colors.selected.background or
                         colors.unselected.background
    sbar.set(env.NAME, {
        icon = {highlight = env.SELECTED},
        label = {highlight = env.SELECTED},
        background = {color = bg_color}
    })
end

local function space_windows_change(env)
    local log = io.open("/Users/shajra/sketchy.log", "a")
    local space, apps = nil, nil
    if env.INFO then
        space = env.INFO.space
        apps = env.INFO.apps
    else
        space = tonumber(env.SPACE)
        apps = load(env.APPS)()
    end
    local app_emojis = {}
    for name, _ in pairs(apps) do
        table.insert(app_emojis, emojis[name] or ":default:")
    end
    icon_strip = "—"
    if next(app_emojis) ~= nil then
        icon_strip = table.concat(app_emojis, " ")
    end
    sbar.animate("tanh", 10, function()
        sbar.set(spaces[space], {label = icon_strip})
    end)
end

for i = 1, 10, 1 do
    local space = sbar.add("space", {
        associated_space = i,
        background = {
            color = colors.unselected.background,
            corner_radius = 6
        },
        icon = {
            string = i,
            padding_left = 10,
            padding_right = 10,
            color = colors.unselected.foreground,
            highlight_color = colors.selected.foreground
        },
        padding_left = 2,
        padding_right = 2,
        label = {
            string = "<>",
            padding_right = 20,
            color = colors.unselected.foreground,
            highlight_color = colors.selected.foreground,
            font = "sketchybar-app-font:Regular:16.0",
            y_offset = -1,
            drawing = true
        }
    })
    spaces[i] = space.name
    space:subscribe("space_change", space_selection)
    space:subscribe("mouse.clicked", mouse_click)
end

sbar.add("bracket", spaces, {})

local space_creator = sbar.add("item", {
    padding_left = 10,
    padding_right = 8,
    icon = {
        string = "􀆊",
        font = {style = "Heavy", size = settings.font.size - 2.0}
    },
    label = {drawing = false},
    associated_display = "active"
})

space_creator:subscribe("mouse.clicked", function(_)
    os.execute("yabai -m space --create && sketchybar --trigger space_change")
end)
space_creator:subscribe("space_windows_change", space_windows_change)
