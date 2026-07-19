-- use scan codes, so 'io' keyboard feels the same

--* main workspaces
--** My.m1
hl.workspace_rule({ workspace = 1, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 3, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 5, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 7, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })

--** My.m2
hl.workspace_rule({ workspace = 2, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 4, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 6, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 8, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })

--** My.m3
hl.workspace_rule({ workspace = 9, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = 10, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })

-- new_on_top: false
-- - true: set windows on top of stack (larger cascade of window movement)

-- new_on_active:none (new wins relative to current focus)
-- - after: less visual cascade
-- - none (default): bottom of stack (unless new_on_top)

-- workspace binds in $XDG_CONFIG_DIR/hypr/$host/bindings/tiling.conf

--* Focus Workspaces

--** My.m1
hl.workspace_rule({ workspace = 11, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 13, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 15, monitor = "desc:" .. My.m1.desc, layout_opts = { orientation = "center", mfact = 0.4, } })

--** My.m2
hl.workspace_rule({ workspace = 12, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 14, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 16, monitor = "desc:" .. My.m2.desc, layout_opts = { orientation = "center", mfact = 0.4, } })

--*** MOD3 + k[1-6]
hl.bind("MOD3 + code:" .. k1, hl.dsp.focus({ workspace = 11 }))
hl.bind("MOD3 + code:" .. k2, hl.dsp.focus({ workspace = 12 }))
hl.bind("MOD3 + code:" .. k3, hl.dsp.focus({ workspace = 13 }))
hl.bind("MOD3 + code:" .. k4, hl.dsp.focus({ workspace = 14 }))
hl.bind("MOD3 + code:" .. k5, hl.dsp.focus({ workspace = 15 }))
hl.bind("MOD3 + code:" .. k6, hl.dsp.focus({ workspace = 16 }))

--*** MOD3 + shift + k[1-6]
hl.bind("MOD3 + SHIFT + code:" .. k1, hl.dsp.window.move({ workspace = 11 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k2, hl.dsp.window.move({ workspace = 12 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k3, hl.dsp.window.move({ workspace = 13 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k4, hl.dsp.window.move({ workspace = 14 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k5, hl.dsp.window.move({ workspace = 15 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k6, hl.dsp.window.move({ workspace = 16 }, { follow = false }))

--** My.m3
hl.workspace_rule({ workspace = 17, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = 18, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = 19, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })
hl.workspace_rule({ workspace = 20, monitor = "desc:" .. My.m3.desc, layout_opts = { orientation = "bottom", } })

--*** MOD3 + k[7890]
hl.bind("MOD3 + code:" .. k7, hl.dsp.focus({ workspace = 17 }))
hl.bind("MOD3 + code:" .. k8, hl.dsp.focus({ workspace = 18 }))
hl.bind("MOD3 + code:" .. k9, hl.dsp.focus({ workspace = 19 }))
hl.bind("MOD3 + code:" .. k0, hl.dsp.focus({ workspace = 20 }))

--*** MOD3 + shift + k[7890]
hl.bind("MOD3 + SHIFT + code:" .. k7, hl.dsp.window.move({ workspace = 17 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k8, hl.dsp.window.move({ workspace = 18 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k9, hl.dsp.window.move({ workspace = 19 }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k0, hl.dsp.window.move({ workspace = 20 }, { follow = false }))

--** My.m4.
hl.workspace_rule({ workspace = 21, monitor = My.m4.port, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 22, monitor = My.m4.port, layout_opts = { orientation = "center", mfact = 0.4, } })
hl.workspace_rule({ workspace = 23, monitor = My.m4.port, layout_opts = { orientation = "center", mfact = 0.4, } })

--*** MOD3 + SUPER + k[123]
hl.bind("MOD3 + SUPER + code:" .. k1, hl.dsp.focus({ workspace = 21 }))
hl.bind("MOD3 + SUPER + code:" .. k2, hl.dsp.focus({ workspace = 22 }))
hl.bind("MOD3 + SUPER + code:" .. k3, hl.dsp.focus({ workspace = 23 }))

--*** MOD3 + SUPER + SHIFT + k[123]
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k1, hl.dsp.window.move({ workspace = 21 }, { follow = false }))
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k2, hl.dsp.window.move({ workspace = 22 }, { follow = false }))
hl.bind("MOD3 + SUPER + SHIFT + code:" .. k3, hl.dsp.window.move({ workspace = 23 }, { follow = false }))

--* default workspaces
hl.workspace_rule({ workspace = 1, monitor = "desc:" .. My.m1.desc, default = true, })
hl.workspace_rule({ workspace = 2, monitor = "desc:" .. My.m2.desc, default = true, })
hl.workspace_rule({ workspace = 9, monitor = "desc:" .. My.m3.desc, default = true, })
hl.workspace_rule({ workspace = 21, monitor = "port:" .. My.m4.port, default = true, })
