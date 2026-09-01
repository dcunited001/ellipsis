--* Man
local ws_man = { name = "man", mod = "MOD3", key = "M" }
local info_title = "¤ INFO"
ws_man.launch = "emacs --init-directory=~/.emacs.info -T \"" ..
    info_title .. "\" -f info-standalone " ..
    "--eval='(load-theme (intern \"wombat\"))'"

--** Binds
dc.ws.binds_special(ws_man.mod, ws_man.key, ws_man.name)

--** Workspace
local wsname = "special:" .. ws_man.name

hl.workspace_rule({
    workspace = wsname,
    border_size = 5,
    on_created_empty = "[float] " .. ws_man.launch
})

--** Rules
hl.window_rule({
    match = { title = "(.*)" .. info_title .. "(.*)" },
    workspace = wsname,
    float = 1,
    size = "1280 720",
    move = "100%-w-5% 5%",
})
