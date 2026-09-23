local omarchy_gdk_scale = 1
local omarchy_monitor_scale = 1

hl.env("GDK_SCALE", tostring(omarchy_gdk_scale))
hl.monitor({ output = "", mode = "preferred", position = "auto", scale = omarchy_monitor_scale })

-- TODO: re-index monitors so they match hyprland indexing
hl.monitor({ output = dc.m1.output, mode = dc.m1.mode, position = "3000x1440", scale = 1.0, supports_hdr = 1 })
hl.monitor({ output = dc.m2.output, mode = dc.m2.mode, position = "3000x0", scale = 1.0 })
hl.monitor({ output = dc.m3.output, mode = dc.m3.mode, position = "1920x480", scale = 1.0, transform = 3 })

dc.m4.mode = dc.m4.res .. "@59.99"
dc.m4.transform = 0

hl.monitor({ output = dc.m4.output, mode = dc.m4.mode, position = "0x960", scale = 1.0 })

-- TODO hdr?
-- hl.config({ quirks = { prefer_hdr = 1 } })
-- hl.monitor({ ... , supports_hdr = 1 })
