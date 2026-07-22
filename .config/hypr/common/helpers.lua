My = My or {}

My.apps = {
    browser = "uwsm app -- chromium",
    browserPrivate = "uwsm app -- chromium --private-window",
    terminal = "uwsm app -- alacritty",
    fileManager = "uwsm app -- thunar",
    editor = "uwsm app -- doomclient -- -c -n"
}

My.icons = {
    hypr       = "indicator-keyboard-Hy-5",
    info       = "dialog-information",
    warn       = "dialog-warning",
    err        = "dialog-error",
    sysd       = "system-run-symbolic",
    screenshot = "accessories-screenshot-symbolic",
    notify     = "preferences-system-notifications-symbolic"
}

My.ws = {}

-- TODO find lua helper to "extend" properties for table references

function My.ws.binds(mod, key, name)
    hl.bind(table.concat({ mod, key }, "+"),
        hl.dsp.focus({ workspace = name }),
        { description = "Toggle WS: " .. name })
    hl.bind(table.concat({ mod, "SHIFT", key }, "+"),
        hl.dsp.window.move({ workspace = name }),
        { description = "Move Win to WS: " .. name })
end

function My.ws.binds_special(mod, key, name)
    hl.bind(table.concat({ mod, key }, "+"),
        hl.dsp.workspace.toggle_special(name),
        { description = "Toggle WS: " .. name })
    hl.bind(table.concat({ mod, "SHIFT", key }, "+"),
        hl.dsp.window.move({ workspace = "special:" .. name }),
        { description = "Move Win to WS: " .. name })
end
