local cjson = require "cjson"
local socket = require("posix.sys.socket")
local unistd = require("posix.unistd")

local json = cjson.new()

local NULL = cjson.null

-- DESIGN: AeroSpace's socket protocol (as of v0.21) begins each connection with
-- a version handshake and frames every message with a 4-byte little-endian
-- length prefix.
local SOCKET_PROTOCOL_VERSION = 1

local function get_user_name()
    local handle = io.popen("id -un")
    if handle then
        local result = handle:read("*a")
        handle:close()
        if result then
            return result:match("^%s*(.-)%s*$") -- Trim whitespace
        else
            return nil
        end
    else
        return nil
    end
end

local function split(text)
    local lines = {}
    for line in text:gmatch("[^\r\n]+") do
        if line:match("%S") then table.insert(lines, line) end
    end
    return lines
end

local function head(strings)
    for _, str in ipairs(strings) do if str:match("%S") then return str end end
    return nil
end

local username = get_user_name()

local DEFAULT_CONFIG = {
    SOCKET_PATH = string.format("/tmp/bobko.aerospace-%s.sock", username)
}

local ERROR_MESSAGES = {
    SOCKET_CREATE = "Failed to create Unix domain socket",
    SOCKET_CONNECT = "Failed to connect to socket at %s",
    SOCKET_SEND = "Failed to send data through socket",
    SOCKET_RECEIVE = "Failed to receive data from socket",
    SOCKET_CLOSE = "Failed to close socket connection",
    SOCKET_NOT_CONNECTED = "Socket is not connected",
    JSON_DECODE = "Failed to decode JSON response",
    PROTOCOL_MISMATCH = "AeroSpace socket protocol version mismatch (restart AeroSpace)",
    INVALID_WORKSPACE = "Invalid workspace identifier provided"
}

local function write_all(fd, data)
    local total = #data
    local sent = 0
    while sent < total do
        local n, err = unistd.write(fd, data:sub(sent + 1))
        if not n then
            error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_SEND,
                                tostring(err)))
        end
        sent = sent + n
    end
    return sent
end

local function read_exactly(fd, count)
    local parts = {}
    local got = 0
    while got < count do
        local chunk, err = unistd.read(fd, count - got)
        if not chunk then
            error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_RECEIVE,
                                tostring(err)))
        end
        if chunk == "" then
            error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_RECEIVE,
                                "connection closed by server"))
        end
        parts[#parts + 1] = chunk
        got = got + #chunk
    end
    return table.concat(parts)
end

local function handshake(fd)
    write_all(fd, string.pack("<I4", SOCKET_PROTOCOL_VERSION))
    local server_version = string.unpack("<I4", read_exactly(fd, 4))
    if server_version ~= SOCKET_PROTOCOL_VERSION then
        error(string.format("%s: client=%d server=%d",
                            ERROR_MESSAGES.PROTOCOL_MISMATCH,
                            SOCKET_PROTOCOL_VERSION, server_version))
    end
end

local Aerospace = {}
Aerospace.__index = Aerospace

function Aerospace.new(socketPath)
    local self = setmetatable({}, Aerospace)
    self.socketPath = socketPath or DEFAULT_CONFIG.SOCKET_PATH

    local fd, err = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM, 0)
    if not fd then
        error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_CREATE,
                            tostring(err)))
    end

    self.fd = fd

    local addr = {family = socket.AF_UNIX, path = self.socketPath}

    if socket.connect(self.fd, addr) ~= 0 then
        unistd.close(fd)
        error(string.format(ERROR_MESSAGES.SOCKET_CONNECT, self.socketPath))
    end

    handshake(self.fd)

    return self
end

function Aerospace:reconnect()
    if self:is_initialized() then self:close() end

    local fd, err = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM, 0)
    if not fd then
        error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_CREATE,
                            tostring(err)))
    end

    self.fd = fd

    local addr = {family = socket.AF_UNIX, path = self.socketPath}

    if socket.connect(self.fd, addr) ~= 0 then
        unistd.close(fd)
        error(string.format(ERROR_MESSAGES.SOCKET_CONNECT, self.socketPath))
    end

    handshake(self.fd)
end

function Aerospace:is_initialized() return self.fd ~= nil end

function Aerospace:send(query)
    if not self:is_initialized() then
        error(ERROR_MESSAGES.SOCKET_NOT_CONNECTED)
    end

    -- Newer AeroSpace servers reject requests that omit these fields, so pass
    -- explicit JSON null when we have no window/workspace context to forward.
    if query.windowId == nil then query.windowId = NULL end
    if query.workspace == nil then query.workspace = NULL end

    local payload = json.encode(query)
    local framed = string.pack("<I4", #payload) .. payload
    return write_all(self.fd, framed)
end

function Aerospace:receive()
    if not self:is_initialized() then
        error(ERROR_MESSAGES.SOCKET_NOT_CONNECTED)
    end

    local length = string.unpack("<I4", read_exactly(self.fd, 4))
    return read_exactly(self.fd, length)
end

function Aerospace:close()
    if self:is_initialized() then
        local ok, err = unistd.close(self.fd)
        if ok ~= 0 then
            error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_CLOSE,
                                tostring(err)))
        end
        self.fd = nil
    end
end

local function decode_response(response)
    local success, result = pcall(json.decode, response)
    if not success then
        error(string.format("%s: %s", ERROR_MESSAGES.JSON_DECODE,
                            tostring(result)))
    end
    return result
end

function Aerospace:list_workspaces(switches, callback)
    local args = {
        "list-workspaces", "--format",
        "%{workspace}%{monitor-appkit-nsscreen-screens-id}", "--json"
    }
    table.move(switches, 1, #switches, #args + 1, args)
    local query = {command = "", args = args, stdin = ""}
    self:send(query)
    local response = decode_response(self:receive())
    local workspaces = decode_response(response.stdout)
    if callback then return callback(workspaces) end
    return workspaces
end

function Aerospace:list_workspace_names(switches, callback)
    local args = {"list-workspaces"}
    table.move(switches, 1, #switches, #args + 1, args)
    local query = {command = "", args = args, stdin = ""}
    self:send(query)
    local response = split(decode_response(self:receive()).stdout)
    if callback then return callback(response) end
    return response
end

function Aerospace:focused_workspace(callback)
    local response = head(self:list_workspace_names({"--focused"}))
    if callback then return callback(response) end
    return response
end

function Aerospace:workspace(workspace)
    if not workspace or type(workspace) ~= "string" then
        error(ERROR_MESSAGES.INVALID_WORKSPACE)
    end

    local query = {command = "", args = {"workspace", workspace}, stdin = ""}

    self:send(query)
    local response = decode_response(self:receive())

    return response.stdout
end

function Aerospace:list_all_windows(callback)
    local query = {
        command = "",
        args = {
            "list-windows", "--all", "--json", "--format",
            "%{window-id}%{app-name}%{window-title}%{workspace}"
        },
        stdin = ""
    }

    self:send(query)
    local response = decode_response(self:receive())
    local windows = decode_response(response.stdout)

    if callback then return callback(windows) end
    return windows
end

Aerospace.__gc = Aerospace.close

return Aerospace
