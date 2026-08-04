-- scratchpad
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]", gaps_in = 12, gaps_out = 36 })
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]" })
hl.workspace_rule({ workspace = "n[e:scratchpad]s[true]", layout_opts = { orientation = "center" } })

hl.bind("SUPER + S", hl.dsp.workspace.toggle_special("scratchpad"),
    { description = "Toggle scratchpad" })
hl.bind("SUPER + ALT + S", hl.dsp.window.move({ workspace = "special:scratchpad" }),
    { description = "Move window to scratchpad" })
