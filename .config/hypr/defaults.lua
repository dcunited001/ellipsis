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
