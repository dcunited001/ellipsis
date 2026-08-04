-- SPDX-FileCopyrightText: 2026 David Heinemeier Hansson
--
-- SPDX-License-Identifier: MIT
--
-- Copyright (c) David Heinemeier Hansson
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:

-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-- Shared helpers for Hyprland Lua configuration.
--
-- https://github.com/basecamp/omarchy/blob/1b6ab15331bfc88eb66746021d9e32c976ed438a/default/hypr/helpers.lua
--
-- Eventually, this will be either:
--
-- - managed in nixos/guix as a package
-- - managed as a compatible interface implementation ... something like that idk

o = o or {}

local function shell_quote(value)
    return "'" .. tostring(value):gsub("'", "'\\''") .. "'"
end

o.shell_quote = shell_quote

local function file_exists(path)
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end

    return false
end

function o.shell_succeeds(command)
    local ok, _, code = os.execute(command .. " >/dev/null 2>&1")
    return ok == true or ok == 0 or code == 0
end

function o.cmd_present(command)
    if command:find("/", 1, true) then
        return file_exists(command)
    end

    local path = os.getenv("PATH") or "/usr/local/bin:/usr/bin"
    for directory in (path .. ":"):gmatch("([^:]*):") do
        if file_exists((directory ~= "" and directory or ".") .. "/" .. command) then
            return true
        end
    end

    return false
end

function o.cmd_missing(command)
    return not o.cmd_present(command)
end

local function command_from(value, description)
    if type(value) ~= "table" then
        return value
    end

    if value.omarchy then
        return "omarchy-launch-" .. value.omarchy
    elseif value.focus and value.launch then
        return o.launch_sole(value.focus, value.launch)
    elseif value.launch then
        return o.launch(value.launch)
    elseif value.webapp then
        if value.focus then
            return o.launch_webapp_sole(description, value.webapp)
        else
            return o.launch_webapp(value.webapp)
        end
    elseif value.tui then
        if value.focus then
            return "omarchy-launch-or-focus-tui " .. shell_quote(value.tui)
        else
            return "omarchy-launch-tui " .. shell_quote(value.tui)
        end
    end

    return value
end

function o.preinstalled_bindings_enabled()
    if _G.omarchy_preinstalled_bindings ~= nil then
        return _G.omarchy_preinstalled_bindings == true
    end

    return not file_exists((os.getenv("HOME") or "") .. "/.local/state/omarchy/preinstalls-removed")
end

function o.bind(keys, description, dispatcher, options)
    local opts = options or {}

    if description then
        opts.description = description
    end

    dispatcher = command_from(dispatcher, description)

    if type(dispatcher) == "string" then
        dispatcher = hl.dsp.exec_cmd(dispatcher)
    end

    hl.bind(keys, dispatcher, opts)
end

function o.launch(command)
    return "uwsm-app -- " .. command
end

function o.exec_on_start(command)
    hl.on("hyprland.start", function()
        hl.exec_cmd(command)
    end)
end

function o.launch_on_start(command)
    o.exec_on_start(o.launch(command))
end

function o.launch_webapp(url)
    return "omarchy-launch-webapp " .. shell_quote(url)
end

function o.launch_webapp_sole(name, url)
    return "omarchy-launch-or-focus-webapp " .. shell_quote(name) .. " " .. shell_quote(url)
end

function o.launch_sole(match, command)
    return "omarchy-launch-or-focus " .. shell_quote(match) .. " " .. shell_quote(o.launch(command))
end

function o.bind_toggle(keys, description, toggle, options)
    o.bind(keys, description, "omarchy-toggle-" .. toggle, options)
end

function o.notify(message)
    return "notify-send -u low " .. shell_quote(message)
end

function o.window(match, rules)
    rules.match = rules.match or {}

    if type(match) == "string" then
        rules.match.class = match
    else
        for key, value in pairs(match) do
            rules.match[key] = value
        end
    end

    hl.window_rule(rules)
end
