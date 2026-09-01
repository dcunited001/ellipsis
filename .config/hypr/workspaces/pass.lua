--* Pass
local ws_pass = { name = "pass", mod = "SUPER + ALT", key = "P" }
ws_pass.launch = "uwsm app -- keepassxc"

-- TODO: hypr: define color for pass workspace

--** Binds
dc.ws.binds_special_ws(ws_pass)

--** Workspace
local wsname = "special:" .. ws_pass.name
hl.workspace_rule({ workspace = wsname, on_created_empty = ws_pass.launch })

--** Rules
for i, c in ipairs({ "keepassxc", "org.keepassxc.KeePassXC", "pwsafe" }) do
    hl.window_rule({ match = { class = c }, tag = "+pass" })

    -- keep no_screen_share here
    hl.window_rule({ match = { class = c }, no_screen_share = 1 })
end

--*** Keepass
for i, c in ipairs({ "keepassxc", "org.keepassxc.KeePassXC" }) do
    hl.window_rule({ match = { class = c, title = "(.* - KeePassXC)" }, size = "1280 720" })
    hl.window_rule({ match = { class = c, title = "^(Generate Password)$" }, size = "960 540" })
end

--*** pwsafe
for i, c in ipairs({ "pwsafe" }) do
    hl.window_rule({ match = { class = c, title = "^(Master Password).*" }, size = "640 360" })
    hl.window_rule({ match = { class = c, title = "^(Password Safe).*(.psafe3)" }, size = "15% 70%", move = "10% 10%" })
end

--*** pinentry
for i, c in ipairs({ "(org.gnupg.pinentry-.*)" }) do
    hl.window_rule({ match = { class = c }, no_screen_share = 1, stay_focused = 1 })
    hl.window_rule({ match = { class = c }, move = "30% 30%" })
    hl.window_rule({ match = { class = c }, border_size = 10 })
end
