--* scratchterm
local ws_term = { name = "scratchterm", mod = "MOD3", key = "space" }

--** Binds
dc.ws.binds_special_ws(ws_term)

--** Workspace
hl.workspace_rule({ workspace = "n[e:scratchterm]s[true]", gaps_in = 12, gaps_out = 36 })
hl.workspace_rule({ workspace = "n[e:scratchterm]s[true]" })
hl.workspace_rule({ workspace = "n[e:scratchterm]s[true]", layout_opts = { orientation = "center" } })
hl.workspace_rule({ workspace = "n[e:scratchterm]s[true]", on_created_empty = "omarchy launch terminal" })
