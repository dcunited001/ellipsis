--* EMAIL
local ws_email = { name = "email", mod = "MOD3", key = "E" }

-- ws_email.border_color = ws_email.border_color
ws_email.launch = "thunderbird --name org.mozilla.Thunderbird -mail -calendar"
ws_email.monitor = dc.m2.output
ws_email.border_color = { colors = { "rgba(113355DD)", "rgba(332299FF)" }, angle = 300 }
ws_email.border_size = 5
local tbird = { class = "(org\\.mozilla\\.)?[Tt]hunderbird" }

--** Binds
dc.ws.binds_ws(ws_email)

--** Workspace
local wsname = "name:" .. ws_email.name

hl.workspace_rule({
    workspace = wsname,
    monitor = ws_email.monitor,
    on_created_empty = "[tile] " .. ws_email.launch
})

--** Rules

-- thunderbird
hl.window_rule({
    match = { class = tbird.class },
    workspace = wsname,
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
        workspace = wsname,
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
        workspace = wsname,
        float = true,
    })
end

hl.window_rule({
    match = { class = tbird.class, title = "(Write: ).*" },
    workspace = wsname,
    float = true,
    size = "1280 720"
})
