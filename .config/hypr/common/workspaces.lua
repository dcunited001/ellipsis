--* main workspaces

-- layout = "lua:manual"
hl.workspace_rule({ workspace = "1", monitor = "desc:" .. dc.m1.desc, default = true, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "2", monitor = "desc:" .. dc.m2.desc, default = true, layout_opts = { orientation = "center", } })

-- use scan codes, so 'io' keyboard feels the same

-- new_on_top: false
-- - true: set windows on top of stack (larger cascade of window movement)

-- new_on_active:none (new wins relative to current focus)
-- - after: less visual cascade
-- - none (default): bottom of stack (unless new_on_top)

-- workspace binds in $XDG_CONFIG_DIR/hypr/$host/bindings/tiling.conf

--** dc.m1
hl.workspace_rule({ workspace = "1", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "3", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "5", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "7", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "9", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })

--** dc.m2
hl.workspace_rule({ workspace = "2", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "4", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "6", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "8", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "10", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })

--* Focus Workspaces

--** dc.m1
hl.workspace_rule({ workspace = "11", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "13", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "15", monitor = "desc:" .. dc.m1.desc, layout_opts = { orientation = "center", } })

--** dc.m2
hl.workspace_rule({ workspace = "12", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "14", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })
hl.workspace_rule({ workspace = "16", monitor = "desc:" .. dc.m2.desc, layout_opts = { orientation = "center", } })

--** binds

--*** MOD3 + k[1-6]
hl.bind("MOD3 + code:" .. k1, hl.dsp.focus({ workspace = "11" }))
hl.bind("MOD3 + code:" .. k2, hl.dsp.focus({ workspace = "12" }))
hl.bind("MOD3 + code:" .. k3, hl.dsp.focus({ workspace = "13" }))
hl.bind("MOD3 + code:" .. k4, hl.dsp.focus({ workspace = "14" }))
hl.bind("MOD3 + code:" .. k5, hl.dsp.focus({ workspace = "15" }))
hl.bind("MOD3 + code:" .. k6, hl.dsp.focus({ workspace = "16" }))

--*** MOD3 + shift + k[1-6]
hl.bind("MOD3 + SHIFT + code:" .. k1, hl.dsp.window.move({ workspace = "11" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k2, hl.dsp.window.move({ workspace = "12" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k3, hl.dsp.window.move({ workspace = "13" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k4, hl.dsp.window.move({ workspace = "14" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k5, hl.dsp.window.move({ workspace = "15" }, { follow = false }))
hl.bind("MOD3 + SHIFT + code:" .. k6, hl.dsp.window.move({ workspace = "16" }, { follow = false }))

--* manage master layout

-- hl.bind("SUPER + CTRL + ALT + SHIFT + S", function()

local function omfgllm()
    local ws = hl.get_active_workspace()
    if hl.get_active_special_workspace() then
        ws = hl.get_active_special_workspace()
    end

    if not ws then return end

    local ws_name = ws.addressable_name

    hl.notification.create({
        text = dump(ws),
        timeout = 5000,
        color = "rgb(FF0000)"
    })

    if ws.special then
        hl.workspace_rule({ workspace = ws_name, layout_opts = { orientation = "top" } })
    else
        hl.workspace_rule({ workspace = ws_name, layout_opts = { orientation = "top" } })
    end
end

hl.bind("MOD3 + S", hl.dsp.layout("orientationcycle top center"))
