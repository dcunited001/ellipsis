--* TUI Tray
local ws_tuitray = { name = "tuitray", mod = "SUPER + SHIFT", key = "Prior" }
ws_tuitray.launch = "isd"
local tuitray = { class = "org.dc.tuitray", prefix = "tuitray" }

--** Binds
My.ws.binds(ws_tuitray.mod, ws_tuitray.key, ws_tuitray.name)

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
local tuitray_mod = "SUPER + SHIFT"
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

    hl.bind(table.concat(v.mod and { tuitray_mod, v.mod } or { tuitray_mod }, "+"),
        hl.dsp.exec_cmd("uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:" .. k .. "' -e " .. k),
        { description = "Launch TUI: " .. k })
end
