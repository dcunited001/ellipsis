--* Org Roam
local roam = { prefix = "♦ DOOM", tab = "¤ ROAM" }
local ws_roam = { name = "roam", mod = "MOD3", key = "backslash" }
ws_roam.size = "1920 1080"


--** Binds
dc.ws.binds_special(ws_roam.mod, ws_roam.key, ws_roam.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_roam.name,
    on_created_empty = "[float] " .. dc.activities_launch(roam.tab)
})

--** Rules
hl.window_rule({
    match = { title = dc.activities_title({ prefix = roam.prefix, tab = roam.tab }) },
    workspace = "special:" .. ws_roam.name,
    float = true,
    size = ws_roam.size,
})

--* Org Agenda
local agenda = { prefix = "♦ DOOM", tab = "¤ AGENDA" }
local ws_agenda = { name = "agenda", mod = "MOD3", key = "home" }
ws_agenda.size = "1920 1080"

--** Binds
dc.ws.binds_special(ws_agenda.mod, ws_agenda.key, ws_agenda.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_agenda.name,
    on_created_empty = "[float] " .. dc.activities_launch(agenda.tab)
})

--** Rules
hl.window_rule({
    match = { title = dc.activities_title({ prefix = agenda.prefix, tab = agenda.tab }) },
    workspace = "special:" .. ws_agenda.name,
    float = true,
    size = ws_agenda.size,
})
