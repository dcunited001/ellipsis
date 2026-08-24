--* TUI Tray
local ws_tuitray = { name = "tuitray", mod = "MOD3", key = "Prior" }
ws_tuitray.launch = "isd"
local tuitray = { class = "org.dc.tuitray", prefix = "tuitray" }

local function tuitray_cmd(tui)
    -- tui class and bin name should match
    return "alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:" .. tui .. "' -e " .. tui
end

--** Binds
dc.ws.binds_special(ws_tuitray.mod, ws_tuitray.key, ws_tuitray.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_tuitray.name,
    monitor = ws_tuitray.monitor,
    -- on_created_empty = "[float] " .. o.launch(tuitray_cmd(ws_tuitray.launch))
})

-- bindd = SUPER SHIFT, D, Docker, exec, $tuiContainers
-- bindd = SUPER SHIFT ALT, D, SystemD, exec, $tuiSysD
-- bindd = SUPER SHIFT, T, Activity, exec, $tuiTop

--** Rules

-- TODO tuitray: rework to use omarchy-launch-or-focus-tui

-- local tuitray_mod = "SUPER + SHIFT"
local tuitray_apps = {
    isd = { size = "75% 75%", move = "5% 5%", mod = "ALT" },
    -- sysz = { size = "1024 768", move = "5% 100%-w-5% ", mod = {} },
    nmtui = { size = "1024 768", move = "100%-w-5% 5%" },
    btop = { size = "75% 75%", move = "100%-w-5% 100%-w-5%" },
    lazydocker = { size = "80% 80%", move = "100%-w-5% 100%-w-5%" },
}

for k, v in pairs(tuitray_apps) do
    hl.window_rule({
        match = {
            class = "^(Alacritty:" .. tuitray.class .. ")",
            title = tuitray.prefix .. ":" .. k
        },
        float = true,
        size = v.size,
        move = v.move
    })
end
