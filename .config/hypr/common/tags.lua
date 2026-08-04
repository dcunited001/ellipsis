-- scratchpad tag
hl.window_rule({ match = { workspace = "n[e:scratchpad]s[true]" }, tag = "+scratchpad" })
hl.window_rule({ match = { tag = "scratchpad*" }, border_size = 8 })
