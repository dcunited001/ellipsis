--* Docs
local ws_docs = { name = "docs", mod = "SUPER", key = "D" }
-- ws_docs.border_color = ws_docs.border_color
ws_docs.launch = "flatpak run org.zealdocs.Zeal"
ws_docs.monitor = My.m2.port

--** Binds
hl.bind(table.concat({ ws_docs.mod, ws_docs.key }, "+"),
    hl.dsp.workspace.toggle_special(ws_docs.name),
    { description = "Toggle WS: " .. ws_docs.name })
hl.bind(table.concat({ ws_docs.mod, "SHIFT", ws_docs.key }, "+"),
    hl.dsp.window.move({ workspace = "special:" .. ws_docs.name }),
    { description = "Move Win to WS: " .. ws_docs.name })

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_docs.name,
    monitor = ws_docs.monitor,
    border_size = 10,
    on_created_empty = "[float] " .. ws_docs.launch
})

--** Rules

-- helvum
hl.window_rule({
    match = { class = "(org.zealdocs.Zeal)" },
    workspace = "special:" .. ws_docs.name,
    float = true,
    size = "1920 1080"
})
