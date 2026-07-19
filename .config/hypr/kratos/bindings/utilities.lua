--* Menus
hl.bind("SUPER + SPACE", hl.dsp.exec_cmd("walker"),
    { description = "Launch Apps" })
hl.bind("SUPER + SHIFT + SPACE", hl.dsp.exec_cmd("walker -m desktopapplications -p Start…"),
    { description = "Launch Apps" })
hl.bind("SUPER + CTRL + E", hl.dsp.exec_cmd("walker -m symbols -p 'Select Emoji...'"),
    { description = "Emoji picker" })
hl.bind("SUPER + ALT + SPACE", hl.dsp.exec_cmd("walker"),
    { description = "Omarchy Menu" })
hl.bind("SUPER + CTRL + W", hl.dsp.exec_cmd("walker -m windows"),
    { description = "Windows" })
hl.bind("SUPER + CTRL + U", hl.dsp.exec_cmd("walker -m unicode -p 'Select Unicode...'"),
    { description = "Unicode picker" })

--* Walker

--* Aesthetics

--* Notifications
local notifyDND = "notify-send -i " .. My.icons.notify .. " SwayNC Notifications $(swaync-client -d)"

hl.bind("MOD3 + COMMA", hl.dsp.exec_cmd("uwsm app -- swaync-client -t"),
    { description = "Toggle SwayNC" })
hl.bind("SUPER + COMMA", hl.dsp.exec_cmd("swaync-client --hide-all"),
    { description = "Hide last notifications" })
hl.bind("SUPER + SHIFT + COMMA", hl.dsp.exec_cmd("swaync-client --close-all"),
    { description = "Close all notifications" })
hl.bind("SUPER + CTRL + COMMA", hl.dsp.exec_cmd(notifyDND),
    { description = "Toggle silencing notifications" })

hl.define_submap("SWAYNC", function()
    hl.bind("space", hl.dsp.exec_cmd("swaync-client -a 1"))
    hl.bind("code:10", hl.dsp.exec_cmd("swaync-client -a 1"))
    hl.bind("code:11", hl.dsp.exec_cmd("swaync-client -a 2"))
    hl.bind("code:12", hl.dsp.exec_cmd("swaync-client -a 3"))
    hl.bind("escape", hl.dsp.submap("reset"))
end)

hl.bind("SUPER + ALT + COMMA", hl.dsp.submap("SWAYNC"), { description = "Invoke last notification" })

--* Toggle idling

hl.bind("SUPER + CTRL + I", hl.dsp.exec_cmd("dc-systemctl-toggle hypridle"), { description = "Toggle locking on idle" })

--* Toggle nightlight

--* Control Apple Display brightness

--* Captures

--* Screen recordings

--* Color picker

--* File sharing

--* Waybar-less information
