hl.config({
    general = {
        resize_on_border = true,
        layout = "master"
    },
    misc = {
        force_default_wallpaper = -1,
        disable_hyprland_logo = false,
        enable_anr_dialog = true,
        anr_missed_pings = 10 -- 20
    },
    master = {
        new_status = "slave",
        allow_small_split = true,
        mfact = 0.4
    }
})

browser = "uwsm app -- chromium"
browserPrivate = "uwsm app -- chromium --private-window"

terminal = "uwsm app -- alacritty"
fileManager = "uwsm app -- thunar"
editor = "uwsm app -- doomclient -- -c -n"

-- tui
tuiContainers = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:lazydocker' -e lazydocker"
tuiTop = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:btop' -e btop"
tuiSysD = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:isd' -e isd"
-- tuiSysD = "alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:sysz' -e sysz"

-- default icons
ihypr = "indicator-keyboard-Hy-5"
iinfo = "dialog-information"
iwarn = "dialog-warning"
ierr = "dialog-error"
isysd = "system-run-symbolic"
iscreenshot = "accessories-screenshot-symbolic"
