local colors = require("colors")
local emojis = require("emojis")
local settings = require("settings")
local spaces = {}
local focused_display = nil
local session_restore = {}
local session_timestamp = {}
local max_display_count = 1

local function mouse_click(env)
    if env.BUTTON == "right" then
        os.execute("yabai -m space --destroy " .. env.SID ..
                       " && sketchybar --trigger space_change")
    else
        os.execute("yabai -m space --focus " .. env.SID)
    end
end

function contains(display_mask, display)
    return display_mask & (1 << display) ~= 0
end

local function space_selection(env)
    local is_selected = env.SELECTED == "true"
    local displays = tonumber(sbar.query(env.NAME).geometry
                                  .associated_display_mask)
    local is_focused = not focused_display or
                           contains(displays, focused_display)
    local bg_color, fg_color
    if is_selected then
        if is_focused then
            bg_color = colors.selected.focused.background
            fg_color = colors.selected.focused.foreground
        else
            bg_color = colors.selected.unfocused.background
            fg_color = colors.selected.unfocused.foreground
        end
    else
        bg_color = colors.unselected.background
        fg_color = colors.unselected.foreground
    end
    local fg_props = {highlight = env.SELECTED, highlight_color = fg_color}
    sbar.set(env.NAME, {
        icon = fg_props,
        label = fg_props,
        background = {color = bg_color}
    })
end

local function space_windows_change(env)
    local space = env.INFO.space
    local apps = env.INFO.apps
    local app_emojis = {}
    for name, _ in pairs(apps) do
        table.insert(app_emojis, emojis[name] or ":default:")
    end
    icon_strip = "—"
    if next(app_emojis) ~= nil then
        icon_strip = table.concat(app_emojis, " ")
    end
    if spaces[space] then
        sbar.animate("tanh", 10, function()
            sbar.set(spaces[space], {label = icon_strip})
        end)
    end
    local handle = assert(io.popen("@display_count@"))
    local display_count = tonumber(assert(handle:read("a")))
    handle:close()
    handle = assert(io.popen("@session_save@"))
    session_restore[display_count] = assert(handle:read("a"))
    session_timestamp[display_count] = os.clock()
    handle:close()
    if display_count > max_display_count then
        max_display_count = display_count
    end
end

local function display_added(display_count) return display_count - 1, 1, -1 end

local function display_removed(display_count)
    return display_count + 1, max_display_count, 1
end

local function display_count_change(range)
    return function(env)
        local restore = nil
        local max_timestamp = nil
        local handle = assert(io.popen("@display_count@"))
        local display_count = tonumber(assert(handle:read("a")))
        handle:close()
        local first, last, step = range(display_count)
        for display_count = first, last, step do
            if session_timestamp[display_count] and
                (max_timestamp == null or session_timestamp[display_count] >
                    max_timestamp) then
                max_timestamp = session_timestamp[display_count]
                restore = session_restore[display_count]
            end
        end
        if restore then os.execute(restore) end
    end
end

local function display_change(env)
    focused_display = tonumber(env.INFO)
    sbar.trigger("space_change")
end

for i = 1, 10, 1 do
    local space = sbar.add("space", "spaces." .. tostring(i), {
        associated_space = i,
        background = {color = colors.unselected.background, corner_radius = 6},
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

sbar.add("bracket", "spaces", spaces, {})

local space_creator = sbar.add("item", "space_creator", {
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
space_creator:subscribe("display_change", display_change)
space_creator:subscribe("display_added", display_count_change(display_added))
space_creator:subscribe("display_removed", display_count_change(display_removed))
