local omarchy_gdk_scale = 1
local omarchy_monitor_scale = 1

hl.env("GDK_SCALE", tostring(omarchy_gdk_scale))
hl.monitor({ output = "", mode = "preferred", position = "auto", scale = omarchy_monitor_scale })

hl.monitor({ output = dc.m1.port, mode = dc.m1.mode, position = "3000x1440", scale = 1.0 })
hl.monitor({ output = dc.m2.port, mode = dc.m2.mode, position = "3000x0", scale = 1.0 })
hl.monitor({ output = dc.m3.port, mode = dc.m3.mode, position = "1920x480", scale = 1.0, transform = 3 })

dc.m4.mode = dc.m4.res .. "@59.99"
dc.m4.transform = 0

hl.monitor({ output = dc.m4.port, mode = dc.m4.mode, position = "0x960", scale = 1.0 })

-- monitor=desc:$m1desc,2560x1440@120.0,0x1440,1.0
-- monitor=desc:$m2desc,2560x1440@59.95,0x0,1.0
-- monitor=desc:$m3desc,1920x1080@60.0,2560x480,1.0
-- monitor=desc:$m3desc,transform,3
--
-- $m3port = HDMI-A-1

--* Layoute
--** no tv

-- hl.monitor({ output = m1port, mode = m1mode, position = "1080x1440", scale = 1.0 })
-- hl.monitor({ output = m2port, mode = m2mode, position = "1080x0", scale = 1.0 })
-- hl.monitor({ output = m3port, mode = m3mode, position = "0x480", scale = 1.0, transform = 3 })

--*** add tv


-- hl.monitor({ output = m4port, mode = m4mode, position = "1080x1920", scale = 1.0 })

-- position = 0x1440   -- m1
-- position = 0x0      -- m2
-- position = 2560x480 -- m3
-- position = 0x2880   -- m4

--** screenshare

-- m4mode = "1280x720@60.00"
-- hl.monitor({ output = m4port, mode = m4mode, position = "1080x1920", scale = 1.0 })

--** tv left
