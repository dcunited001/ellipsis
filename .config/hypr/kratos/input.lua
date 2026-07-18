local xkb_opts = {
    "caps:ctrl_modifier",
    "lv3:ralt_alt",
    "altwin:menu_win",
    "mylvl5:ins_switch",
    "mylvl3:sclk_switch",
    "menu2:xf86calculator_menu" }

-- works on dell pc104: mylvl3:lctl_switch,underspace:altgr_underscore
-- { "fmedia:voldown_f18", "fmedia:mute_f17" }

-- doesn't work with my layout
-- "menu2:xf86calculator_menu"

hl.config({
    input = {
        -- io works, but in hyprland, digit binds need code:10 through code:19
        kb_layout = "us",
        kb_variant = "altgr-intl",
        kb_model = "pc104",
        kb_options = table.concat(xkb_opts, ","),

        -- numlock_by_default = on
        follow_mouse = true,

        repeat_rate = 25,
        repeat_delay = 400,
        touchpad = {
            natural_scroll = true,
            tap_to_click = true,
            disable_while_typing = true,
        },
        sensitivity = 0 -- -1.0 - 1.0, 0 means no modification.
    }
})
