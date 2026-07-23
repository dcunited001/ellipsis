local function activities_title(act)
    return "(" .. act.prefix .. ")(.*)(" .. act.tab .. ")(.*)"
end

local function activities_launch(tab, socket)
    return "uwsm app -- doomclient " .. (socket and "-s " .. socket or "")
        .. " -- -ce '(activities-resume (cdr (assoc \""
        .. tab .. "\" activities-activities)))'"
end

--* Emacs
local ws_dfemacs = { name = "emacs", mod = "MOD3", key = "backspace" }
ws_dfemacs.border_size = 5
local dfemacs = { prefix = "♦ DOOM", tab = "df¶\\.doom\\.d" }

--** Binds
My.ws.binds_special(ws_dfemacs.mod, ws_dfemacs.key, ws_dfemacs.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_dfemacs.name,
    border_size = ws_dfemacs.border_size,
    on_created_empty = activities_launch(dfemacs.prefix)
})

--** Rules
local emacs_tabs = { "df¶\\.doom\\.d", "df¶\\.emacs\\.doom" }
emacs_tabs = My.elmap(emacs_tabs, function(el)
    return activities_title({ prefix = dfemacs.prefix, tab = el })
end)

for i, title in ipairs(emacs_tabs) do
    hl.window_rule({
        match = { title = title },
        workspace = "special:" .. ws_dfemacs.name,
        size = "1920 1080",
        float = true
    })
end

--* Hypr
local ws_dfhypr = { name = "hypr", mod = "MOD3", key = "equal" }
ws_dfhypr.border_size = 5
local dfhypr = { prefix = "♦ DOOM", tab = "df¶hypr" }

--** Binds
My.ws.binds_special(ws_dfhypr.mod, ws_dfhypr.key, ws_dfhypr.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_dfhypr.name,
    border_size = ws_dfhypr.border_size,
    on_created_empty = activities_launch(dfhypr.prefix)
})

--** Rules
hl.window_rule({
    match = { title = title },
    workspace = "special:" .. ws_dfhypr.name,
    size = "1920 1080",
    float = true
})

--* Nixos
local ws_dfnixos = { name = "nixos", mod = "MOD3", key = "return" }
ws_dfnixos.border_size = 5
local dfnixos = { prefix = "♦ DOOM", tab = "df¶nixos" }

--** Binds
My.ws.binds_special(ws_dfnixos.mod, ws_dfnixos.key, ws_dfnixos.name)

--** Workspace
hl.workspace_rule({
    workspace = "special:" .. ws_dfnixos.name,
    border_size = ws_dfnixos.border_size,
    on_created_empty = activities_launch(dfnixos.prefix)
})

--** Rules
hl.window_rule({
    match = { title = title },
    workspace = "special:" .. ws_dfnixos.name,
    size = "1920 1080",
    float = true
})
