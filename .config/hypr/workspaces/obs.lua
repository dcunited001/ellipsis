--* OBS
-- on kharis, pause; on kratos f11 (also the pause button)
local ws_obs = { name = "obs", mod = "MOD3", key = "F11" }
ws_obs.size = "90% 90%"
ws_obs.launch = "obs"
ws_obs.monitor = My.m3.port

local obs_class = "com.obsproject.Studio"

-- note: not a special workspace

--** Binds
hl.bind(table.concat({ ws_obs.mod, ws_obs.key }, "+"),
    hl.dsp.focus({ focus = ws_obs.name }),
    { description = "Toggle WS: " .. ws_obs.name })
hl.bind(table.concat({ ws_obs.mod, "SHIFT", ws_obs.key }, "+"),
    hl.dsp.window.move({ workspace = ws_obs.name }),
    { description = "Move Win to WS: " .. ws_obs.name })

--** Workspace
-- windowrule = workspace name:$wsObs, match:class $wsObsClass
hl.workspace_rule({
    workspace = ws_obs.name,
    monitor = ws_obs.monitor,
    on_created_empty = "[float] " .. ws_obs.launch,
    border = true,
    bordersize = 16
})


--** Rules
hl.window_rule({
    match = { class = obs_class },
    tile = true
})

-- OBS doesn't detect XF86 keysyms... does this interfere?
-- https://wiki.hypr.land/Configuring/Uncommon-tips--tricks/#set-f13-f24-as-usual-function-keys
