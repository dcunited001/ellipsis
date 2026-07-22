--* TUI Tray
local ws_tuitray = { name = "tuitray", mod = "SUPER + SHIFT", key = "Prior" }
ws_tuitray.launch = "isd"
local tuitray = { class = "org.dc.tuitray", prefix = "tuitray" }

--** Binds
My.ws.binds(ws_tuitray.name, ws_tuitray.mod, ws_tuitray.key)

--** Workspace
hl.workspace_rule({
    workspace = ws_tuitray.name,
    monitor = ws_tuitray.monitor,
    on_created_empty = "[float] " .. ws_tuitray.launch
})

-- bindd = SUPER SHIFT, D, Docker, exec, $tuiContainers
-- bindd = SUPER SHIFT ALT, D, SystemD, exec, $tuiSysD
-- bindd = SUPER SHIFT, T, Activity, exec, $tuiTop

--** Rules
local tuitray_mod = { "SUPER + SHIFT" }
local tuitray_apps = {
    isd = { size = "size 75% 75%", move = "move 5% 5%", mod = { "ALT" } },
    -- sysz = { size = "size 1024 768", move = "move 5% 100%-w-5% ", mod = {} },
    nmtui = { size = "size 1024 768", move = "move 100%-w-5% 5%", mod = {} },
    btop = { size = "size 75% 75%", move = "move 100%-w-5% 100%-w-5%", mod = {} },
    lazydocker = { size = "80% 80%", move = "move 100%-w-5% 100%-w-5%", mod = {} },
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

    hl.bind(table.concat({ tuitray_mod, v.mod }, "+"),
        hl.dsp.exec_cmd("uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:" .. k .. "' -e " .. k),
        { description = "Launch TUI: " .. k })
end
