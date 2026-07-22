--* Man
local ws_man = { name = "man", mod = "MOD3", key = "M" }
local info_title = "¤ INFO"
ws_man.launch = 'emacs -T \"" .. info_title .. "\" -f info-standalone --eval="(load-theme (intern \"wombat\"))"'

--** Binds
hl.bind(table.concat({ ws_man.mod, ws_man.key }, "+"),
    hl.dsp.workspace.toggle_special(ws_man.name),
    { description = "Toggle WS: " .. ws_man.name })
hl.bind(table.concat({ ws_man.mod, "SHIFT", ws_man.key }, "+"),
    hl.dsp.window.move({ workspace = "special:" .. ws_man.name }),
    { description = "Move Win to WS: " .. ws_man.name })

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_man.name,
    border_size = 5,
    on_created_empty = "[float] " .. ws_man.launch
})

--** Rules
hl.window_rule({
    match = { title = "(.*)" .. info_title .. "(.*)" },
    workspace = "special:" .. ws_man.name,
    float = 1,
    size = "1280 720",
    move = "100%-w-5% 5%",
})
