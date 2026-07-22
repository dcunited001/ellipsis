My = My or {}

My.apps = {
    browser = "uwsm app -- chromium",
    browserPrivate = "uwsm app -- chromium --private-window",
    terminal = "uwsm app -- alacritty",
    fileManager = "uwsm app -- thunar",
    editor = "uwsm app -- doomclient -- -c -n"
}

My.tui = {
    tuiContainers = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:lazydocker' -e lazydocker",
    tuiTop = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:btop' -e btop",
    tuiSysD = "uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:isd' -e isd"
}

My.icons = {
    hypr       = "indicator-keyboard-Hy-5",
    info       = "dialog-information",
    warn       = "dialog-warning",
    err        = "dialog-error",
    sysd       = "system-run-symbolic",
    screenshot = "accessories-screenshot-symbolic",
    notify     = "preferences-system-notifications-symbolic"
}
