--* Org Roam
local roam_tab = "¤ ROAM"
local roam_title = "(♦ DOOM)(.*)(¤ ROAM)(.*)"
local ws_roam = { name = "roam", mod = "MOD3", key = "backslash" }
ws_roam.size = "1920 1080"
ws_roam.launch = "uwsm app -- doomclient -- -ce '(activities-resume (cdr (assoc \"" ..
    roam_tab .. "\" activities-activities)))'"

--** Binds
My.ws.binds_special(ws_roam.mod, ws_roam.key, ws_roam.name)

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
My.ws.binds_special(ws_agenda.mod, ws_agenda.key, ws_agenda.name)

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
