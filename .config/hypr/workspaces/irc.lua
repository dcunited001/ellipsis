local ws_irc = { name = "irc", mod = "MOD3", key = "F9" }
ws_irc.launch = "quassel"

local hyprhost = os.getenv("HYPRHOST")
if hyprhost == "kharis" then
    ws_irc.monitor = dc.m2.output
elseif hyprhost == "kratos" then
    ws_irc.monitor = dc.m3.output
end

--* IRC

--** Binds
dc.ws.binds_special(ws_irc.mod, ws_irc.key, ws_irc.name)

--** Workspace
local wsname = "special:" .. ws_irc.name

hl.workspace_rule({
    workspace = wsname,
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
        workspace = wsname,
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
