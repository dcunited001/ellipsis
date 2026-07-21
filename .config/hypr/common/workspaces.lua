-- scratchpad
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]", gaps_in = 12, gaps_out = 36 })
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]" })
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]", layout_opts = { orientation = "center" } })

hl.bind("SUPER + S", hl.dsp.workspace.toggle_special("scratchpad"),
    { description = "Toggle scratchpad" })
hl.bind("SUPER + ALT + S", hl.dsp.window.move({ workspace = "special:scratchpad" }),
    { description = "Move window to scratchpad" })

-- scratchpad2
hl.workspace_rule({ workspace = "n[e:scratchpad2]s[true]", gaps_in = 12, gaps_out = 36 })
hl.workspace_rule({ workspace = "n[e:scratchpad2]s[true]" })
hl.workspace_rule({ workspace = "n[e:scratchpad2]s[true]", layout_opts = { orientation = "center" } })

hl.bind("MOD3 + S", hl.dsp.workspace.toggle_special("scratchpad2"),
    { description = "Toggle scratchpad2" })
hl.bind("MOD3 + SHIFT + S", hl.dsp.window.move({ workspace = "special:scratchpad2" }),
    { description = "Move window to scratchpad2" })

-- scratchpad tag
hl.window_rule({ match = { workspace = "n[e:scratchpad2]s[true]" }, tag = "scratchpad" })
hl.window_rule({ match = { workspace = "n[e:scratchpad]s[true]" }, tag = "+scratchpad" })
hl.window_rule({ match = { tag = "scratchpad*" }, border_size = 8 })
