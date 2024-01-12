local icons = require("icons")

local battery = sbar.add("item", {
    position = "right",
    label = {drawing = false},
    update_freq = 120
})

local function battery_update()
    local handle = assert(io.popen("pmset -g batt"))
    local batt_info = assert(handle:read("a"))
    handle:close()
    local icon = "!"

    if (string.find(batt_info, 'AC Power')) then
        icon = icons.battery.charging
    else
        local found, _, charge = batt_info:find("(%d+)%%")
        if found then charge = tonumber(charge) end

        if found and charge > 80 then
            icon = icons.battery._100
        elseif found and charge > 60 then
            icon = icons.battery._75
        elseif found and charge > 40 then
            icon = icons.battery._50
        elseif found and charge > 20 then
            icon = icons.battery._25
        else
            icon = icons.battery._0
        end
    end

    battery:set({icon = icon})
end

battery:subscribe({"routine", "power_source_change", "system_woke"},
                  battery_update)
