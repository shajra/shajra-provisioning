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
    local cmd =
        "/System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -I"
    local handle = assert(io.popen(cmd))
    local report = assert(handle:read("a"))
    handle:close()
    local ssid = report:match("[ \n]+SSID: *([^ \n]+)")
    if ssid then
        handle = assert(io.popen("ipconfig getifaddr en0"))
        local ip = assert(handle:read("a"))
        handle:close()
        wifi:set({
            icon = icons.wifi.connected,
            label = ssid .. " (" .. ip .. ")"
        })
        respect_verbosity()
    else
        wifi:set({
            icon = icons.wifi.disconnected,
            label = {string = "", width = 0}
        })
    end
end)

wifi:subscribe("mouse.clicked", function(env)
    if wifi:query().label.value ~= "" then
        verbose = not verbose
        respect_verbosity()
    end
end)
