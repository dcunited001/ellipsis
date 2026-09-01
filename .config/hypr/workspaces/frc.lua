--* FRC

local ws_robot_sim = { name = "frcSim", mod = "SUPER", key = "F8" }
local ws_ascope = { name = "frcAscope", mod = "SUPER", key = "F9" }
local ws_ascope_float = { name = "frcAscopeFloat", mod = "SUPER", key = "F10" }

-- ws_ascope.launch = o.launch_webapp("https://discord.com/channels/@me");

--** Monitors

local hyprhost = os.getenv("HYPRHOST")
if hyprhost == "kharis" then
    ws_robot_sim.monitor = dc.m2.output
    ws_ascope.monitor = dc.m2.output
    ws_ascope_float.monitor = dc.m2.output
elseif hyprhost == "kratos" then
    ws_robot_sim.monitor = dc.m2.output
    ws_ascope.monitor = dc.m2.output
    ws_ascope_float.monitor = dc.m2.output
end

--** Binds
dc.ws.binds_ws(ws_robot_sim)
dc.ws.binds_ws(ws_ascope)
dc.ws.binds_ws(ws_ascope_float)

-- NOTE: render_unfocused is necessary, otherwise AdvantageScope graphs
-- offscreen will cause the main interface to lag

--** AdvantageScope Windows
local wsname = "name:" .. ws_ascope.name

ws_ascope.border_color = {
    colors = { "rgba(CC3333DD)", "rgba(BBCC7777)" },
    angle = 115
}

local ascope_class = "AdvantageScope"
local ascope_suffix = "— AdvantageScope"

hl.window_rule({
    match = {
        class = ascope_class,
        title = "(.* Help " .. ascope_suffix .. ")"
    },
    float = true,
    size = "720 720"
})

hl.window_rule({ match = { title = ascope_class .. "(.*)" }, tag = "+ascope" })
hl.window_rule({ match = { tag = "ascope" }, workspace = wsname })

hl.window_rule({
    match = { tag = "ascope" },
    border_color = ws_ascope.border_color,
    border_size = 5,
    -- render_unfocused = true
})

--** Satellite Windows
--
-- Requires some metadata in the JSON (@)
wsname = "name:" .. ws_ascope_float.name
ws_ascope_float.border_color = {
    colors = { "rgba(AA3377DD)", "rgba(77CCFF77)" },
    angle = 115
}

local ascope_sat_suffix = "@ — AdvantageScope"
hl.window_rule({
    match = { title = "(.*)" .. ascope_sat_suffix },
    tag = "+ascopeSat"
})

hl.window_rule({
    match = { tag = "ascopeSat" },
    workspace = wsname,
    float = true,
    render_unfocused = true
})

hl.window_rule({
    match = { tag = "ascopeSat" },
    border_color = ws_ascope_float.border_color,
    border_size = 8
})

-- windowrule = workspace name:$wsFrcAscopeFloat, match:tag ascopeSatellite

--* Misc Rules

--** by class
hl.window_rule({ match = { class = "^(wpical)(.*)$" }, float = true })
hl.window_rule({ match = { class = "^(edu.wpi.first.pathweaver)(.*)$" }, float = true })
hl.window_rule({ match = { class = "^(edu-wpi-first-smartdashboard)(.*)$" }, float = true })
hl.window_rule({ match = { class = "^(edu.wpi.first.shuffleboard)(.*)$" }, float = true })
hl.window_rule({ match = { class = "(Datalog Tool)" }, float = true })
hl.window_rule({ match = { class = "(elastic_dashboard)" }, float = true })

--** by title
hl.window_rule({ match = { title = "(Glass -)(.*)$" }, float = 1 })
hl.window_rule({ match = { title = "(System Identification)(.*)$" }, float = 1 })
hl.window_rule({ match = { title = "(OutlineViewer -)(.*)$" }, float = 1 })
hl.window_rule({ match = { title = "(roboRIO Team Number)(.*)$" }, float = 1 })
