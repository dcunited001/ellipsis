-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
-- List current monitors and supported resolutions with: hyprctl monitors all

local omarchy_gdk_scale = 1
local omarchy_monitor_scale = 1

-- TODO: re-index monitors so they match hyprland indexing
hl.env("GDK_SCALE", tostring(omarchy_gdk_scale))
hl.monitor({ output = "", mode = "preferred", position = "auto", scale = omarchy_monitor_scale })

-- -- Portrait/rotated secondary monitor (transform: 1 = 90°, 3 = 270°).
-- -- hl.monitor({ output = "DP-2", mode = "preferred", position = "auto", scale = 1, transform = 1 })

hl.monitor({
    output = dc.m1.output,
    mode = dc.m1.mode,
    position = "0x1080",
    transform = dc.m1.transform,
    scale = dc.m1.scale
})
hl.monitor({
    output = dc.m2.output,
    mode = dc.m2.mode,
    position = "0x0",
    transform = dc.m2.transform,
    scale = dc.m2.scale
})
