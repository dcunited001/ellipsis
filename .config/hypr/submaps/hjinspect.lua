local sub_hji = { name = "HJINSPECT", mod = "SUPER + ALT", key = "F1" }
local hji_params = '-1 10000 "rgb(CCCC00)"'
local hji_clip = "hyprctl activewindow -j | hjinspect.jq | wl-copy"
local hji_notify = "hyprctl notify " .. hji_params .. ' "$(hyprctl activewindow -j | hjinspect.jq)"'

hl.bind(sub_hji.mod .. "+" .. sub_hji.key, hl.dsp.submap(sub_hji.name))

hl.define_submap(sub_hji.name, function()
    hl.bind("code:" .. k1, hl.dsp.exec_cmd(hji_notify))
    hl.bind("F1", hl.dsp.exec_cmd(hji_clip))
    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("catchall", hl.dsp.submap("reset"))
end)
