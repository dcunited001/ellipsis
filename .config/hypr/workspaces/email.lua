--* EMAIL
local ws_email = { name = "email", mod = "MOD3", key = "E" }
-- ws_email.border_color = ws_email.border_color
ws_email.launch = "thunderbird --name org.mozilla.Thunderbird -mail -calendar"
ws_email.monitor = My.m2.port
ws_email.border_color = { colors = { "rgba(113355DD)", "rgba(332299FF)" }, angle = 300 }
ws_email.border_size = 5
local tbird = { class = "(org\\.mozilla\\.)?[Tt]hunderbird" }


--** Binds
hl.bind(table.concat({ ws_email.mod, ws_email.key }, "+"),
    hl.dsp.focus({ workspace = ws_email.name }),
    { description = "Toggle WS: " .. ws_email.name })
hl.bind(table.concat({ ws_email.mod, "SHIFT", ws_email.key }, "+"),
    hl.dsp.window.move({ workspace = ws_email.name }),
    { description = "Move Win to WS: " .. ws_email.name })

--** Workspace
hl.workspace_rule({
    workspace = ws_email.name,
    monitor = ws_email.monitor,
    on_created_empty = "[float] " .. ws_email.launch
})

--** Rules

-- thunderbird
hl.window_rule({
    match = { class = tbird.class },
    workspace = ws_email.name,
    border_color = ws_email.border_color,
    border_size = ws_email.border_size
})

local tbird_small_titles = {
    "(Select Calendar)$",
    "([0-9]+ Reminder[s]?)$",
    "(Calendar Reminder[s]?)$"
}

for i, title in ipairs(tbird_small_titles) do
    hl.window_rule({
        match = { class = tbird.class, title = title },
        workspace = ws_email.name,
        float = true,
        size = "640 360",
    })
end

tbird_floats = {
    "^()$",
    "^(OpenPGP .*)",
    "(Message Filters)",
}

for i, title in ipairs(tbird_floats) do
    hl.window_rule({
        match = { class = tbird.class, title = title },
        workspace = ws_email.name,
        float = true,
    })
end

hl.window_rule({
    match = { class = tbird.class, title = "(Write: ).*" },
    workspace = ws_email.name,
    float = true,
    size = "1280 720"
})
