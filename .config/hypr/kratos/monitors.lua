My.m1 = {
    port = "DP-1",
    desc = "LG Electronics LG ULTRAGEAR 103NTGYHB375",
    res = "2560x1440",
    transform = 0,
}
My.m1.mode = My.m1.res .. "@60.00"

My.m2 = {
    port = "DP-2",
    desc = "Samsung Electric Company U28D590",
    res = "2560x1440",
    transform = 0,
}
My.m2.mode = My.m2.res .. "@60.00"

My.m3 = {
    port = "DP-3",
    desc = "ViewSonic Corporation VG2239 Series UBW154602336",
    res = "1920x1080",
    transform = 0,
}
My.m3.mode = My.m3.res .. "@60.00"

My.m4 = {
    port = "HDMI-A-1",
    res = "1920x1080",
    transform = 0,
}
My.m4.mode = My.m4.res .. "@30.00"

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

hl.monitor({ output = My.m1.port, mode = My.m1.mode, position = "3000x1440", scale = 1.0 })
hl.monitor({ output = My.m2.port, mode = My.m2.mode, position = "3000x0", scale = 1.0 })
hl.monitor({ output = My.m3.port, mode = My.m3.mode, position = "1920x480", scale = 1.0, transform = 3 })

My.m4.mode = My.m4.res .. "@59.99"
My.m4.transform = 0

hl.monitor({ output = My.m4.port, mode = My.m4.mode, position = "0x960", scale = 1.0 })
