--* Forum
local ws_forum = { name = "forum", mod = "MOD3", key = "minus" }
ws_forum.border_size = 5

--** Binds
My.ws.binds_special(ws_forum.mod, ws_forum.key, ws_forum.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_forum.name,
    border_size = 5
})

--** Rules
local chrome_apps = {
    chiefdelphi = { title = "Chief Delphi", size = "960 960", move = "100%-w-5% 100%-w-5%" },
    systemcrafters = { title = "System Crafters", size = "960 960", move = "100%-w-5% 5%" },
    nixos = { title = "NixOS Discourse", size = "960 960", move = "5% 5%" },
}

for k, app in pairs(chrome_apps) do
    hl.window_rule({
        match = { class = "(chrome-.*)$", title = "(" .. app.title .. ".*)$" },
        float = true,
        size = app.size,
        move = app.move
    })
end
