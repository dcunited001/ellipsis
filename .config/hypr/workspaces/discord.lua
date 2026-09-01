--* Discord
local ws_discord = { name = "discord", mod = "SUPER + ALT", key = "Delete" }
ws_discord.size = "90% 90%"
ws_discord.launch = o.launch_webapp("https://discord.com/channels/@me");

local wsname = "name:" .. ws_discord.name

local hyprhost = os.getenv("HYPRHOST")
if hyprhost == "kharis" then
    ws_discord.monitor = dc.m2.output
elseif hyprhost == "kratos" then
    ws_discord.monitor = dc.m3.output
end

--** Binds
dc.ws.binds_ws(ws_discord)

--** Workspace
hl.workspace_rule({
    workspace = wsname,
    monitor = ws_discord.monitor,
    border_size = 5,
    on_created_empty = "[tile] " .. ws_discord.launch,
})

--** Rules

-- local discord_classes = { "(chrome-discord.com.*)", "discord", "vesktop" }

-- for i, klass in ipairs(discord_classes) do
--     hl.window_rule({
--         match = { class = klass },
--         workspace = wsname,
--         size = ws_discord.size,
--         move = "10% 10%",
--     })
-- end
