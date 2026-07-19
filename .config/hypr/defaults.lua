hl.config({
    general = {
        resize_on_border = true,
        layout = "master"
    },
    binds = {
        workspace_back_and_forth = true
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

hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })

-- default icons
My = {
    apps = {
        browser = "uwsm app -- chromium",
        browserPrivate = "uwsm app -- chromium --private-window",
        terminal = "uwsm app -- alacritty",
        fileManager = "uwsm app -- thunar",
        editor = "uwsm app -- doomclient -- -c -n"
    },
    tui = {
        tuiContainers = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:lazydocker' -e lazydocker",
        tuiTop = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:btop' -e btop",
        tuiSysD = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:isd' -e isd"
    },
    icons =
    {
        hypr       = "indicator-keyboard-Hy-5",
        info       = "dialog-information",
        warn       = "dialog-warning",
        err        = "dialog-error",
        sysd       = "system-run-symbolic",
        screenshot = "accessories-screenshot-symbolic",
        notify     = "preferences-system-notifications-symbolic"
    }
}
