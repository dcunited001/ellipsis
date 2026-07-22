--* Discord

local ws_discord = { name = "discord", mod = "SUPER + ALT", key = "Delete" }
ws_discord.size = "90% 90%"
ws_discord.launch = "uwsm app -- chromium --app=\"https://discord.com/channels/@me\""

--** Binds
My.ws.binds_special(ws_discord.mod, ws_discord.key, ws_discord.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_discord.name,
    border_size = 5,
    on_created_empty = "[float] " .. ws_discord.launch
})

--** Rules

local discord_classes = { "(chrome-discord.com.*)", "discord", "vesktop" }

for i, klass in ipairs(discord_classes) do
    hl.window_rule({
        match = { class = klass },
        workspace = "special:" .. ws_discord.name,
        size = ws_discord.size,
        move = "10% 10%"
    })
end
