local colors = require("colors")
local icons = require("icons")

local graph_width = 75
local graph_y_offset = 5
local event = "cpu_update"

sbar.exec("exec @sketchybar_cpu@ " .. event .. " 4.0")

local cpu_user = sbar.add("graph", "cpu.user", graph_width, {
    position = "right",
    width = 0, -- subsequent items will overlap
    graph = {color = colors.blue, fill_color = colors.blue, line_width = 1},
    y_offset = graph_y_offset,
    icon = {drawing = false},
    label = {drawing = false}
})

local cpu_system = sbar.add("graph", "cpu.system", graph_width, {
    position = "right",
    width = graph_width,
    graph = {
        color = colors.red,
        fill_color = colors.transparent(colors.red),
        line_width = 1
    },
    y_offset = graph_y_offset,
    icon = {drawing = false},
    label = {drawing = false}
})

local cpu_load = sbar.add("item", "cpu.load", {
    position = "right",
    y_offset = 2,
    icon = icons.cpu,
    label = "CPU%"
})

local function urgency_color(current_load)
    if current_load >= 70 then
        return colors.urgent
    elseif current_load >= 30 then
        return colors.warning
    elseif current_load >= 10 then
        return colors.info
    else
        return colors.good
    end
end

local function cpu_update(env)
    cpu_load:set({
        label = {
            string = env.total_load .. "%",
            color = urgency_color(tonumber(env.total_load))
        }
    })
    cpu_user:push({tonumber(env.user_load) / 100})
    cpu_system:push({tonumber(env.sys_load) / 100})
end

cpu_load:subscribe(event, cpu_update)
