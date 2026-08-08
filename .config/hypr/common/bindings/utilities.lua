--* Other utilities
--** Icon Search

-- TODO: hypr icon search: ensure deps or change iconSearch per-host
local iconSearch = "uwsm app -- nwg-icon-picker"
local iconSearch2 = "uwsm app -- yad-icon-browser"

hl.bind("SUPER + CTRL + SHIFT + E", hl.dsp.exec_cmd(iconSearch), { description = "NWG Icon Picker" })
hl.window_rule({ name = "iconSearch", match = { class = "nwg-icon-picker" }, float = true, size = "640 720" })

hl.bind("SUPER + CTRL + SHIFT + ALT + E", hl.dsp.exec_cmd(iconSearch2), { description = "YAD Icon Browser" })
hl.window_rule({ name = "iconSearch2", match = { class = "yad-icon-browser" }, float = true, size = "720 1280" })

--** DConf GUI
local dconfClass = "dconf-editor"
hl.bind("SUPER + CTRL + SHIFT + D", hl.dsp.exec_cmd("uwsm app -- dconf-editor"), { description = "DConf Editor" })
hl.window_rule({ name = "iconSearch2", match = { class = dconfClass }, float = true, size = "1280 720" })
