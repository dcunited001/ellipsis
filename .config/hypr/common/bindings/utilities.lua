--* Omarchy Utilities
--** Menus

--** Aesthetics

--** Notifications

--** Toggle idling

--** Toggle nightlight

--** Control Apple Display brightness

--** Captures
hl.bind("SUPER + Print", hl.dsp.exec_cmd("omarchy-cmd-screenshot smart clipboard"),
    { description = "Screenshot: Smart" })
hl.bind("SUPER + ALT + Print", hl.dsp.exec_cmd("omarchy-cmd-screenshot smart"),
    { description = "Screenshot: Smart" })
hl.bind("SUPER + SHIFT + Print", hl.dsp.exec_cmd("omarchy-cmd-screenshot windows"),
    { description = "Screenshot: Monitor" })
hl.bind("SUPER + CTRL + Print", hl.dsp.exec_cmd("omarchy-cmd-screenshot fullscreen"),
    { description = "Screenshot: Monitor" })

--** Screen recordings

--** File sharing

--** Waybar-less information

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
