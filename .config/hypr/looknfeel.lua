-- env = HYPRCURSOR_SIZE,24
-- # env = HYPRCURSOR_THEME,McMojave
-- env = XCURSOR_SIZE,24
-- # env = XCURSOR_THEME,McMojave

hl.config({
    general = {
        resize_on_border = true,
        layout = "master",
        gaps_in = 5,
        gaps_out = 10,
        border_size = 2,
        col = {
            active_border = {
                colors = { "rgba(51,204,255,0.9)", "rgba(0,255,153,0.9)" },
                angle = 45
            },
            inactive_border = "rgba(595959aa)"
        }
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

-- hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })

hl.config({
    decoration = {
        rounding = 8,
        rounding_power = 2,
        active_opacity = 1.0,
        inactive_opacity = 1.0,
        dim_inactive = true,
        dim_strength = 0.04,

        shadow = {
            enabled = true,
            range = 4,
            render_power = 3,
            color = "rgba(1a1a1aee)"
        },

        blur = {
            enabled = false,
            size = 3,
            passes = 1,
            vibrancy = 0.1696
        }
    },
    animations = {
        enabled = true,
    }
})

hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1.00 }, { 0.32, 1.00 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1.00 } } })
hl.curve("linear", { type = "bezier", points = { { 0.00, 0.00 }, { 1.00, 1.00 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.50, 0.50 }, { 0.75, 1.00 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0.00 }, { 0.10, 1.00 } } })

hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn", enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
