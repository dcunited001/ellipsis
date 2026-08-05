dc = dc or {}
dc.host = os.getenv("HYPRHOST") or "kharis"

dc.apps = {
    browser = "chromium",
    browser_private = "chromium --private-window",
    terminal = "alacritty",
    file_manager = "thunar",
    editor = "doomclient -- -c -n"
}

dc.icons = {
    hypr       = "indicator-keyboard-Hy-5",
    info       = "dialog-information",
    warn       = "dialog-warning",
    err        = "dialog-error",
    sysd       = "system-run-symbolic",
    screenshot = "accessories-screenshot-symbolic",
    notify     = "preferences-system-notifications-symbolic"
}
