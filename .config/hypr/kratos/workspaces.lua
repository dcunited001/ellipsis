--* default workspaces
hl.workspace_rule({ workspace = "30", monitor = "desc:" .. dc.m3.desc, default = true, })
hl.workspace_rule({ workspace = "31", monitor = dc.m4.output, default = true, })

--* main workspaces

--** dc.m1

--** dc.m2

--** dc.m3
hl.workspace_rule({ workspace = "27", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "28", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "29", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "30", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })

--** dc.m4

--* Focus Workspaces
--** dc.m1
--** dc.m2
--** dc.m3
--** dc.m4

--*** MOD3 + k[7890]
hl.bind("MOD3 + code:" .. k7, hl.dsp.focus({ workspace = "27" }))
hl.bind("MOD3 + code:" .. k8, hl.dsp.focus({ workspace = "28" }))
hl.bind("MOD3 + code:" .. k9, hl.dsp.focus({ workspace = "29" }))
hl.bind("MOD3 + code:" .. k0, hl.dsp.focus({ workspace = "30" }))

--*** MOD3 + shift + k[7890]
hl.bind("MOD3 + SHIFT + code:" .. k7, hl.dsp.window.move({ workspace = "27" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k8, hl.dsp.window.move({ workspace = "28" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k9, hl.dsp.window.move({ workspace = "29" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k0, hl.dsp.window.move({ workspace = "30" })) -- , { follow = false }))

--** dc.m4
hl.workspace_rule({ workspace = "31", monitor = dc.m4.output, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "32", monitor = dc.m4.output, layout_opts = { orientation = "top", } })
hl.workspace_rule({ workspace = "33", monitor = dc.m4.output, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "34", monitor = dc.m4.output, layout_opts = { orientation = "top", } })

--*** MOD3 + SUPER + k[123]
hl.bind("MOD3 + code:" .. k1, hl.dsp.focus({ workspace = "31" }))
hl.bind("MOD3 + code:" .. k2, hl.dsp.focus({ workspace = "32" }))
hl.bind("MOD3 + code:" .. k3, hl.dsp.focus({ workspace = "33" }))
hl.bind("MOD3 + code:" .. k4, hl.dsp.focus({ workspace = "34" }))

--*** MOD3 + SUPER + SHIFT + k[123]
hl.bind("MOD3 + SHIFT + code:" .. k1, hl.dsp.window.move({ workspace = "31" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k2, hl.dsp.window.move({ workspace = "32" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k3, hl.dsp.window.move({ workspace = "33" })) -- , { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k4, hl.dsp.window.move({ workspace = "34" })) -- , { follow = false }))
