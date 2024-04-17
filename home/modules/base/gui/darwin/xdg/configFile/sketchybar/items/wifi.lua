local colors = require("colors")
local icons = require("icons")

local wifi = sbar.add("item", "wifi", {
    position = "right",
    icon = icons.wifi.disconnected,
    label = {width = "dynamic"}
})

-- DESIGN: This prevents a silly wiggle moving between the dynamic and
-- non-dynamic rendering of an empty string.
local verbose = true

local function respect_verbosity()
    local width = 0
    local details = wifi:query().label.value
    if verbose and details ~= "" then width = "dynamic" end
    wifi:set({label = {width = width}})
end

wifi:subscribe("wifi_change", function(env)
    sbar.exec("ipconfig getsummary en0", function(report)
        local ssid = report:match("[ \n]+SSID *: *([^ \n]+)")
        if ssid then
            sbar.exec("ipconfig getifaddr en0", function(ip)
                wifi:set({
                    icon = icons.wifi.connected,
                    label = ssid .. " (" .. ip .. ")"
                })
                respect_verbosity()
            end)
        else
            wifi:set({
                icon = icons.wifi.disconnected,
                label = {string = "", width = 0}
            })
        end
    end)
end)

wifi:subscribe("mouse.clicked", function(env)
    if wifi:query().label.value ~= "" then
        verbose = not verbose
        respect_verbosity()
    end
end)
