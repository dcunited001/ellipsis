local ws_irc = { name = "irc", mod = "MOD3", key = "F9" }
ws_irc.monitor = My.m3.port
ws_irc.launch = "quassel"

--* IRC

--** Binds
My.ws.binds_special(ws_irc.mod, ws_irc.key, ws_irc.name)

--** Workspace
hl.workspace_rule({
    workspace = ws_irc.name,
    monitor = ws_irc.monitor,
    on_created_empty = "[float] " .. ws_irc.launch
})

--** Rules
local qclass = "(org.kde.quassel)"
local qtiles = {
    "(Quassel IRC .* Quassel IRC)",
    "(Quassel IRC .*)$"
}

for i, title in ipairs(qtiles) do
    hl.window_rule({
        match = { class = qclass, title = title },
        workspace = "special:" .. ws_irc.name,
        tile = true,
    })
end

local qfloats = "(.* — Quassel IRC)"

for i, title in ipairs(qfloats) do
    hl.window_rule({
        match = { class = qclass, title = title },
        float = true
    })
end
