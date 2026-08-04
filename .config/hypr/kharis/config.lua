dc = dc or {}

-- prints 1\n2\n
--
-- for index, value in pairs(hl.get_monitors()) do
--     print(index)
-- end

-- access directly with hl.get_monitors().1.position.x

--* Monitors
dc.m1 = {
    port = "eDP-1",
    desc = "LG Electronics LG ULTRAGEAR 103NTGYHB375",
    res = "1920x1080",
    hz = "60.00",
    transform = 0,
    scale = 1,
}

dc.m1.mode = dc.m1.res .. "@" .. dc.m1.hz

dc.m2 = {
    port = "HDMI-A-1",
    desc = "Acer Technologies Acer H233H LFS0W0104300",
    res = "1920x1080",
    hz = "60.00",
    transform = 0,
    scale = 1,
}

dc.m2.mode = dc.m2.res .. "@" .. dc.m2.hz

--** Associations
