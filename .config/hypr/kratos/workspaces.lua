--* default workspaces
hl.workspace_rule({ workspace = "9", monitor = "desc:" .. dc.m3.desc, default = true, })
hl.workspace_rule({ workspace = "21", monitor = dc.m4.output, default = true, })

--* main workspaces

--** dc.m1

--** dc.m2

--** dc.m3
hl.workspace_rule({ workspace = "9", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "10", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })

--* Focus Workspaces

--** dc.m1

--** dc.m3
hl.workspace_rule({ workspace = "17", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "18", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "19", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = "20", monitor = "desc:" .. dc.m3.desc, layout_opts = { orientation = "bottom", } })

--*** MOD3 + k[7890]
hl.bind("MOD3 + code:" .. k7, hl.dsp.focus({ workspace = "17" }))
hl.bind("MOD3 + code:" .. k8, hl.dsp.focus({ workspace = "18" }))
hl.bind("MOD3 + code:" .. k9, hl.dsp.focus({ workspace = "19" }))
hl.bind("MOD3 + code:" .. k0, hl.dsp.focus({ workspace = "20" }))

--*** MOD3 + shift + k[7890]
hl.bind("MOD3 + SHIFT + code:" .. k7, hl.dsp.window.move({ workspace = "17" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k8, hl.dsp.window.move({ workspace = "18" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k9, hl.dsp.window.move({ workspace = "19" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k0, hl.dsp.window.move({ workspace = "20" }, { follow = false }))

--** dc.m4
hl.workspace_rule({ workspace = "21", monitor = dc.m4.output, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = "22", monitor = dc.m4.output, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = "23", monitor = dc.m4.output, layout_opts = { orientation = "center", mfact = 0.4, } })

--*** MOD3 + SUPER + k[123]
hl.bind("MOD3 + SUPER + code:" .. k1, hl.dsp.focus({ workspace = "21" }))
hl.bind("MOD3 + SUPER + code:" .. k2, hl.dsp.focus({ workspace = "22" }))
hl.bind("MOD3 + SUPER + code:" .. k3, hl.dsp.focus({ workspace = "23" }))

--*** MOD3 + SUPER + SHIFT + k[123]
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k1, hl.dsp.window.move({ workspace = "21" }, { follow = false }))
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k2, hl.dsp.window.move({ workspace = "22" }, { follow = false }))
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k3, hl.dsp.window.move({ workspace = "23" }, { follow = false }))
