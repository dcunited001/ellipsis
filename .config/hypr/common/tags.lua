--* Tags

--** +scratchterm
-- hl.window_rule({ match = { workspace = "n[e:scratchpad]s[true]" }, tag = "+scratchpad" })
-- hl.window_rule({ match = { tag = "scratchpad*" }, border_size = 8 })

--** +pass
local ws_pass = { border_color = { colors = { "rgba(CC3333DD)", "rgba(7722DDAA)" }, angle = 45 } }
hl.window_rule({ match = { tag = "pass" }, no_screen_share = 1, float = 1, border_color = ws_pass.border_color })
