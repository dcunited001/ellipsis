--* Waydroid
local ws_waydroid = { name = "waydroid", mod = "MOD3", key = "A" }

--** Binds
My.ws.binds_special(ws_waydroid.mod, ws_waydroid.key, ws_waydroid.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_waydroid.name,
    border_size = 5
})

--** Rules
hl.window_rule({
    match = { class = "(Waydroid)" },
    float = true,
    size = "800 1280",
    move = "10% 10%"
})
