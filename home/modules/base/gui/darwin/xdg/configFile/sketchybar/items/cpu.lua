local colors = require("colors")
local icons = require("icons")

local graph_width = 75
local graph_y_offset = 5

-- DESIGN: Lua API doesn't yet support adding graphs
-- https://github.com/FelixKratz/SbarLua/issues/6
os.execute("sketchybar --add graph cpu.user right " .. graph_width)
sbar.set("cpu.user", {
    width = 0,  -- subsequent items will overlap
    graph = {
        color = colors.blue,
        fill_color = colors.blue,
        line_width = 1,
    },
    y_offset = graph_y_offset,
    icon = {drawing = false},
    label = {drawing = false},
})

-- DESIGN: Lua API doesn't yet support adding graphs
-- https://github.com/FelixKratz/SbarLua/issues/6
os.execute("sketchybar --add graph cpu.sys right " .. graph_width)
sbar.set("cpu.sys", {
    width = graph_width,
    graph = {
        color = colors.red,
        fill_color = colors.transparent(colors.red),
        line_width = 1,
    },
    y_offset = graph_y_offset,
    icon = {drawing = false},
    label = {drawing = false},
})

sbar.add("item", "cpu.percent", {
    position = "right",
    y_offset=2,
    icon = icons.cpu,
    label = "CPU%",
    update_freq = 4,
    mach_helper = "git.felix.cpu"
})

sbar.add("item", "cpu.top", {
    position = "right",
    padding_right = 15,
    icon = {drawing = false},
    label = ""})
