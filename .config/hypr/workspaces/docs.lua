--* Docs
local ws_docs = { name = "docs", mod = "SUPER + ALT", key = "D" }
-- ws_docs.border_color = ws_docs.border_color
ws_docs.launch = "flatpak run org.zealdocs.Zeal"
ws_docs.monitor = dc.m2.output

--** Binds
dc.ws.binds_special(ws_docs.mod, ws_docs.key, ws_docs.name)

--** Workspace
local wsname = "special:" .. ws_docs.name

hl.workspace_rule({
    workspace = wsname,
    monitor = ws_docs.monitor,
    border_size = 10,
    on_created_empty = "[float] " .. ws_docs.launch
})

--** Rules

--*** Zeal
hl.window_rule({
    match = { class = "(org.zealdocs.Zeal)" },
    workspace = wsname,
    float = true,
    size = "1920 1080"
})

hl.window_rule({
    match = { class = "(chrome-omarchy.org__manual.*)" },
    workspace = wsname,
    float = true,
    size = { 1120, 1050 }
})

hl.window_rule({
    match = { class = "(chrome-search.nixos.org__.*)" },
    workspace = wsname,
    float = true,
    size = { 1120, 1050 }
})
