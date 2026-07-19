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
-- hl.bind("SUPER + SHIFT + mouse:" .. g502btn9, hl.dsp.window.resize({ x = 1680, y = 1050, window = "active" }))

-- [1920,1280] => [1920*1/2, 1080*3/4] == [960, 960]
