o = o or {}

-- refs: o.command_from(), o.launch_on_start(), o.launch_webapp{,_sole}(), o.launch_solo(), o.notify()
local function shell_quote(value)
    return "'" .. tostring(value):gsub("'", "'\\''") .. "'"
end

-- refs: o.cmd_present(), o.preinstalled_bindings_enabled()
local function file_exists(path)
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end

    return false
end

-- function o.shell_succeeds(command)
-- function o.cmd_present(command)
-- function o.cmd_missing(command)

-- local function command_from()
--
-- pass only non-table arguments to o.bind(): (1) a string or (2) a dispatcher

function o.preinstalled_bindings_enabled()
    return false
end

-- function o.launch_webapp(url) -- note: kratos gets old version of this cmd
-- function o.launch_webapp_sole(url)

function o.launch_webapp(url)
    return "uwsm-app -- chromium --app=" .. shell_quote(url)
end

-- TODO: fix omarchy-launch-webapp{,-or-focus} and desktop files...
-- TODO: update pins for omarchy commands on nixos (and add omarchy-launch-or-focus)
function o.launch_webapp_sole(name, url)
    return "uwsm-app -- chromium --app=" .. shell_quote(url)
end

function o.launch_sole(match, command)
    return "omarchy-launch-or-focus " .. shell_quote(match) .. " " .. shell_quote(o.launch(command))
end

function o.bind_toggle(keys, description, toggle, options)
    o.bind(keys, description, o.notify("omarchy-toggle not implemented: " .. description), options)
end

-- function o.notify(message)
-- function o.window(match, rules)
