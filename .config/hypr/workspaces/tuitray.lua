--* TUI Tray
local ws_tuitray = { name = "tuitray", mod = "MOD3", key = "Prior" }
ws_tuitray.launch = "isd"
local tuitray = { class = "org.omarchy.tuitray", prefix = "tuitray" }

-- works to launch simple commands
local function tuitray_cmd(tui)
    return "omarchy-launch-tui --app-id='org.omarchy." .. tui .. "' --title='tuitray:" .. tui .. "' " .. tui

    -- OLD
    -- tui class and bin name should match
    -- return "alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:" .. tui .. "' -e " .. tui
end

--** Binds
dc.ws.binds_special(ws_tuitray.mod, ws_tuitray.key, ws_tuitray.name)

--** Workspace
local wsname = "special:" .. ws_tuitray.name

hl.workspace_rule({
    workspace = wsname,
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

-- TODO: need to add class/title prefix so TUIs can be intended to launch on a
-- to the "tuitray" workspace

for k, v in pairs(tuitray_apps) do
    hl.window_rule({
        match = {
            class = "^(org.omarchy." .. k .. ")",
            -- title = tuitray.prefix .. ":" .. k
        },
        float = true,
        size = v.size,
        move = v.move
    })
end

-- NEW
--
-- Title: Alacritty (Alacritty)
-- Class: org.omarchy.btop (org.omarchy.btop)
-- Workspace -96 (special:hypr)
-- On Monitor 3
-- ([875,600]) @ ([523,1193])

-- OLD
--
-- Title: tuitray:isd (tuitray:isd)
-- Class: Alacritty:org.dc.tuitray (Alacritty:org.dc.tuitray)
-- Workspace -87 (special:tuitray)
-- On Monitor 3
-- ([800,600]) @ ([560,1193])

-- for k, v in pairs(tuitray_apps) do
--     hl.window_rule({
--         match = {
--             class = "^(Alacritty:" .. tuitray.class .. ")",
--             title = tuitray.prefix .. ":" .. k
--         },
--         float = true,
--         size = v.size,
--         move = v.move
--     })
-- end
