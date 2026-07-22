--* Org Roam
local roam_tab = "¤ ROAM"
local roam_title = "(♦ DOOM)(.*)(¤ ROAM)(.*)"
local ws_roam = { name = "roam", mod = "MOD3", key = "backslash" }
ws_roam.size = "1920 1080"
ws_roam.launch = "uwsm app -- doomclient -- -ce '(activities-resume (cdr (assoc \"" ..
    roam_tab .. "\" activities-activities)))'"

--** Binds
hl.bind(table.concat({ ws_roam.mod, ws_roam.key }, "+"),
    hl.dsp.workspace.toggle_special(ws_roam.name),
    { description = "Toggle WS: " .. ws_roam.name })
hl.bind(table.concat({ ws_roam.mod, "SHIFT", ws_roam.key }, "+"),
    hl.dsp.window.move({ workspace = "special:" .. ws_roam.name }),
    { description = "Move Win to WS: " .. ws_roam.name })

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_roam.name,
    on_created_empty = "[float] " .. ws_roam.launch
})

--** Rules
hl.window_rule({
    match = { title = roam_title },
    workspace = "special:" .. ws_roam.name,
    float = true,
    size = ws_roam.size,
})

--* Org Agenda
local agenda_tab = "¤ AGENDA"
local agenda_title = "(♦ DOOM)(.*)(¤ AGENDA)(.*)"
local ws_agenda = { name = "agenda", mod = "MOD3", key = "home" }
ws_agenda.size = "1920 1080"
ws_agenda.launch = "uwsm app -- doomclient -- -ce '(activities-resume (cdr (assoc \"" ..
    agenda_tab .. "\" activities-activities)))'"

--** Binds
hl.bind(table.concat({ ws_agenda.mod, ws_agenda.key }, "+"),
    hl.dsp.workspace.toggle_special(ws_agenda.name),
    { description = "Toggle WS: " .. ws_agenda.name })
hl.bind(table.concat({ ws_agenda.mod, "SHIFT", ws_agenda.key }, "+"),
    hl.dsp.window.move({ workspace = "special:" .. ws_agenda.name }),
    { description = "Move Win to WS: " .. ws_agenda.name })

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_agenda.name,
    on_created_empty = "[float] " .. ws_agenda.launch
})

--** Rules
hl.window_rule({
    match = { title = agenda_title },
    workspace = "special:" .. ws_agenda.name,
    float = true,
    size = ws_agenda.size,
})
