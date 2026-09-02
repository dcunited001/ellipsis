-- Keep only your personal input overrides here. Uncommented settings below
-- replace Omarchy's defaults.
local xkb_opts = {
    "caps:ctrl_modifier",
    "lv3:ralt_alt",
    "altwin:menu_win",
    "mylvl3:sclk_latch",
    -- "mylvl5:kpen_latch",
    "mylvl5:kpen_hyper_latch",
    -- "mylvl5:display_latch",
    "mylvl5:ins_latch"
}

hl.config({
    input = {
        -- io works, but in hyprland, digit binds need code:10 through code:19
        kb_layout = "us",
        kb_variant = "altgr-intl",
        kb_model = "pc104",
        kb_options = table.concat(xkb_opts, ","),

        numlock_by_default = true,
        follow_mouse = true,
        accel_profile = "flat",

        repeat_rate = 40,
        repeat_delay = 600,
        sensitivity = 0,

        touchpad = {
            natural_scroll = true,
            clickfinger_behavior = true,
            tap_to_click = true,
            scroll_factor = 0.4,
            disable_while_typing = false,
            drag_3fg = 1,
        },
    },
    gestures = {
        workspace_swipe_create_new = true,
        workspace_swipe_distance = 500,
        workspace_swipe_invert = true,
        workspace_swipe_touch = true,
        workspace_swipe_touch_invert = true,
        workspace_swipe_min_speed_to_force = 30,
    }
})

-- App-specific touchpad scroll speeds.
o.window("(Alacritty|kitty|foot)", { scroll_touchpad = 1.5 })
o.window("com.mitchellh.ghostty", { scroll_touchpad = 0.2 })

hl.gesture({ fingers = 4, direction = "left", scale = 2.0, action = "move" });
hl.gesture({ fingers = 4, direction = "right", scale = 2.0, action = "move" });
hl.gesture({ fingers = 4, direction = "up", scale = 1.0, action = "float" });
hl.gesture({ fingers = 4, direction = "down", scale = 1.0, action = "float" });

-- hl.gesture({ fingers = 4, direction = "up", mods = "SUPER ALT", action = "moveoutofgroup" });
-- hl.gesture({ fingers = 4, direction = "left", mods = "SUPER ALT", action = "moveintogroup" });
-- hl.gesture({ fingers = 4, direction = "right", mods = "SUPER ALT", action = "moveintogroup" });

-- bind = SUPER ALT CTRL, G, lockactivegroup, toggle
-- gesture = 4, down, mod: SUPER ALT, dispatcher, lockactivegroup, lock
-- gesture = 2, pinchout, mod: SUPER ALT, dispatcher, lockactivegroup, lock
-- gesture = 2, pinchin, mod: SUPER ALT, dispatcher, lockactivegroup, toggle

-- Enable touchpad gestures for changing workspaces.
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Gestures/
-- hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

-- Enable touchpad gestures for moving focus (helpful on scrolling layout).
-- hl.gesture({ fingers = 3, direction = "left", action = function() hl.dispatch(hl.dsp.focus({ direction = "l" })) end })
-- hl.gesture({ fingers = 3, direction = "right", action = function() hl.dispatch(hl.dsp.focus({ direction = "r" })) end })
