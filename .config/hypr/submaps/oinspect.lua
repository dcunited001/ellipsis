local sub_oi = { name = "OINSPECT", mod = "MOD3", key = "F12" }

local oi_path = "echo \"$PATH\" | wl-copy"
local oi_xdg_home = "echo \"$XDG_DATA_HOME\" | wl-copy"
local oi_xdg_data = "echo \"$XDG_DATA_DIRS\" | wl-copy"
local oi_params = '-1 10000 "rgb(CC00CC)"'

-- function oi_notify(cmd, params)
--   -- eugh strncpy
--   return "hyprctl notify " .. oi_params .. ' "$(' .. cmd )"'
-- end

hl.bind(sub_oi.mod .. "+" .. sub_oi.key, hl.dsp.submap(sub_oi.name))

hl.define_submap(sub_oi.name, function()
    hl.bind("P", hl.dsp.exec_cmd(oi_path))
    hl.bind("X", hl.dsp.exec_cmd(oi_xdg_data))
    hl.bind("Shift + X", hl.dsp.exec_cmd(oi_xdg_home))
    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("catchall", hl.dsp.submap("reset"))
end)
