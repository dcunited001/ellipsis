--* g502 mouse
local g502name = "singing-gundi"
local g502profile = 0

--** profile 0,1
-- profile 0,1
-- $g502btn6=279
-- $g502btn9=280

--** profile3
local g502btn6 = 277
local g502btn9 = 280

--* bindings

--** button 6
--*** drag windows
hl.bind("mouse:" .. g502btn6, hl.dsp.window.drag(), { mouse = true })

--*** toggle opacity
hl.bind("SUPER + mouse:" .. g502btn6, hl.dsp.window.tag({ tag = "testopacity" }), { mouse = true })
hl.bind("SUPER + SHIFT + mouse:" .. g502btn6, hl.dsp.window.tag({ tag = "testopacity2" }), { mouse = true })

hl.window_rule({ name = "opacity", match = { tag = "testopacity" }, opacity = 0.7 })
hl.window_rule({ name = "opacity2", match = { tag = "testopacity2" }, opacity = 0.5 })

--*** test blur

--*** test xray
-- hl.bind("SUPER + mouse:" .. g502btn9, hl.dsp.window.set_prop({ prop = "xray", value = true }))

--** button 9
hl.bind("SUPER + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 1920, y = 1080, window = "active" }))
hl.bind("SUPER + SHIFT + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 1280, y = 720, window = "active" }))
hl.bind("SUPER + CTRL + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 960, y = 960, window = "active" }))
hl.bind("SUPER + CTRL + SHIFT + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 1120, y = 1050, window = "active" }))

--* Minimize With Special Workspace
local ws_minimized = { name = "minimized", tag = "minimized" }

local tagmin = "tag:" .. ws_minimized.tag
local wsmin = "special:" .. ws_minimized.name

hl.workspace_rule({ workspace = wsmin, gaps_in = 20, gaps_out = 20 })
hl.window_rule({
    -- workspace = wsmin,
    match = { tag = ws_minimized.tag },
    border_color = { colors = { "rgba(CCBB55FF)" } },
    border_size = 5,
})

local function active_window_is_minimized()
    local window = hl.get_active_window()
    if not window then
        return false
    end

    -- TODO: find out how local scoping works for closures wrapping a local function
    -- (this function is available by name in toggle_minimized() ... so)
    return (window.workspace.name == "special:minimized")
end

-- note: these will get "stuck" if tags are inverse
local function toggle_minimized()
    if active_window_is_minimized() then
        -- hl.dispatch(hl.dsp.window.move({ workspace = hl.get_active_workspace(), window = tagmin }))
        -- hl.dispatch(hl.dsp.window.clear_tags({ window = tagmin }))
        hl.dispatch(hl.dsp.window.tag({ tag = "-" .. ws_minimized.tag, window = hl.get_active_window() }))
        hl.dispatch(hl.dsp.window.move({ workspace = hl.get_active_workspace() })) -- window = hl.get_active_window
    else
        hl.dispatch(hl.dsp.window.tag({ tag = "+" .. ws_minimized.tag, window = hl.get_active_window() }))
        hl.dispatch(hl.dsp.window.move({ workspace = wsmin, follow = false }))
    end
end

-- with mouse (mods were reversed, but easier to avoid accidental state change like this)
hl.bind("MOD3 + mouse:" .. g502btn9, hl.dsp.workspace.toggle_special(ws_minimized.name))
hl.bind("MOD3+ SHIFT + mouse:" .. g502btn9, toggle_minimized)

-- with keys
hl.bind("MOD3 + escape", hl.dsp.workspace.toggle_special(ws_minimized.name))
hl.bind("MOD3 + SHIFT + escape", toggle_minimized)


-- hl.bind("MOD3 + mouse:" .. g502btn9, hl.dsp.window.tag({ tag = "ws12" }))
-- hl.bind("MOD3 + SHIFT + mouse:" .. g502btn9, hl.dsp.window.tag({ tag = "ws14" }))

-- hl.window_rule({
--     workspace = "12",
--     match = { tag = "ws12" },
--     border_color = { colors = { "rgba(CCBB55FF)" } },
--     border_size = 12
-- })
-- hl.window_rule({
--     match = { tag = "ws14" },
--     border_color = { colors = { "rgba(55BBCCFF)" } },
--     border_size = 14
-- })

-- hl.bind("SUPER + SHIFT + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 1680, y = 1050, window = "active" }))

-- [1920,1280] => [1920*1/2, 1080*3/4] == [960, 960]
