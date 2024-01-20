local icons = require("icons")

local battery = sbar.add("item", {
    position = "right",
    label = {width = "dynamic"},
    update_freq = 120
})

local function toggle_percentage()
    local width = battery:query().label.width
    if width > 0 then
        width = 0
    else
        width = "dynamic"
    end
    battery:set({label = {width = width}})
end

local function battery_update()
    local handle = assert(io.popen("pmset -g batt"))
    local batt_info = assert(handle:read("a"))
    handle:close()
    local plug_icon = ""
    -- DESIGN: possible because using AlDente to charge-limit
    if string.find(batt_info, 'AC attached; not charging') then
        plug_icon = icons.plug.not_charging
    elseif string.find(batt_info, '; charging') then
        plug_icon = icons.plug.charging
    end

    local label = ""
    local battery_icon = icons.battery.unknown_charge
    local charge_found, _, charge = batt_info:find("(%d+)%%")
    if charge_found then
        label = charge
        charge = tonumber(charge)
        if charge > 80 then
            battery_icon = icons.battery.charge_at_100
        elseif charge > 60 then
            battery_icon = icons.battery.charge_at_75
        elseif charge > 40 then
            battery_icon = icons.battery.charge_at_50
        elseif charge > 20 then
            battery_icon = icons.battery.charge_at_25
        else
            icon = icons.battery.charge_at_0
        end
    end
    battery:set({icon = plug_icon .. battery_icon, label = label .. "%"})
end

battery:subscribe({"routine", "power_source_change", "system_woke"},
                  battery_update)

battery:subscribe("mouse.clicked", toggle_percentage)
