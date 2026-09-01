--* OBS
-- on kharis, pause; on kratos f11 (also the pause button)
local ws_obs = { name = "obs", mod = "MOD3", key = "F11" }
ws_obs.size = "90% 90%"
ws_obs.launch = "obs"
ws_obs.monitor = dc.m3.output

local obs_class = "com.obsproject.Studio"

-- note: not a special workspace

--** Binds
dc.ws.binds_ws(ws_obs)

--** Workspace
local wsname = "name:" .. ws_obs.name

-- windowrule = workspace name:$wsObs, match:class $wsObsClass
hl.workspace_rule({
    workspace = wsname,
    monitor = ws_obs.monitor,
    on_created_empty = "[float] " .. ws_obs.launch,
    border_size = 16
})

--** Rules
--*** OBS
hl.window_rule({
    match = { class = obs_class, title = "(^Properties for)" },
    float = true
})

-- OBS doesn't detect XF86 keysyms... does this interfere?
-- https://wiki.hypr.land/Configuring/Uncommon-tips--tricks/#set-f13-f24-as-usual-function-keys

--*** GPU Screen Recorder GTK
local gsr_class = "com.dec05eba.gpu_screen_recorder"
hl.window_rule({
    match = { class = gsr_class },
    float = true
})
