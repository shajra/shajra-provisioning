local cjson = require "cjson"
local socket = require("posix.sys.socket")
local unistd = require("posix.unistd")

local json = cjson.new()

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
    SOCKET_PATH = string.format("/tmp/bobko.aerospace-%s.sock", username),
    MAX_BUFFER_SIZE = 2048,
    EXTENDED_BUFFER_SIZE = 4096
}

local ERROR_MESSAGES = {
    SOCKET_CREATE = "Failed to create Unix domain socket",
    SOCKET_CONNECT = "Failed to connect to socket at %s",
    SOCKET_SEND = "Failed to send data through socket",
    SOCKET_RECEIVE = "Failed to receive data from socket",
    SOCKET_CLOSE = "Failed to close socket connection",
    SOCKET_NOT_CONNECTED = "Socket is not connected",
    JSON_DECODE = "Failed to decode JSON response",
    INVALID_WORKSPACE = "Invalid workspace identifier provided"
}

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
end

function Aerospace:is_initialized() return self.fd ~= nil end

function Aerospace:send(query)
    if not self:is_initialized() then
        error(ERROR_MESSAGES.SOCKET_NOT_CONNECTED)
    end

    local encoded_query = json.encode(query)
    encoded_query = encoded_query .. "\n"
    local bytes_sent, err = unistd.write(self.fd, encoded_query)

    if not bytes_sent then
        error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_SEND, tostring(err)))
    end

    return bytes_sent
end

function Aerospace:receive(maxBytes)
    if not self:is_initialized() then
        error(ERROR_MESSAGES.SOCKET_NOT_CONNECTED)
    end

    maxBytes = maxBytes or DEFAULT_CONFIG.MAX_BUFFER_SIZE
    local response, err = unistd.read(self.fd, maxBytes)

    if not response then
        error(string.format("%s: %s", ERROR_MESSAGES.SOCKET_RECEIVE,
                            tostring(err)))
    end

    return response
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
    local response = decode_response(self:receive(
                                         DEFAULT_CONFIG.EXTENDED_BUFFER_SIZE))
    local workspaces = decode_response(response.stdout)
    if callback then return callback(workspaces) end
    return workspaces
end

function Aerospace:list_workspace_names(switches, callback)
    local args = {"list-workspaces"}
    table.move(switches, 1, #switches, #args + 1, args)
    local query = {command = "", args = args, stdin = ""}
    self:send(query)
    local response = split(decode_response(self:receive(
                                               DEFAULT_CONFIG.MAX_BUFFER_SIZE)).stdout)
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
    local response = decode_response(
                         self:receive(DEFAULT_CONFIG.MAX_BUFFER_SIZE))

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
    local response = decode_response(self:receive(
                                         DEFAULT_CONFIG.EXTENDED_BUFFER_SIZE))
    local windows = decode_response(response.stdout)

    if callback then return callback(windows) end
    return windows
end

Aerospace.__gc = Aerospace.close

return Aerospace
